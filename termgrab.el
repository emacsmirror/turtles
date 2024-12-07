;;; termgrab.el --- Screen-grabbing test utility -*- lexical-binding: t -*-

;; Copyright (C) 2024 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 0.1snapshot
;; Package-Requires: ((emacs "29.1"))
;; Keywords: testing, unix
;; URL: http://github.com/szermatt/termgrab

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

;;; Commentary:
;;
;; This package contains utilities for testing the appearance of a
;; buffer or window in the terminal.
;;

;;; Code:

(require 'pcase)
(require 'server)
(require 'ansi-color)

(defcustom termgrab-tmux-exe "tmux"
  "Path to the tmux executable."
  :type 'string
  :group 'termgrab)

(defcustom termgrab-emacsclient-exe "emacsclient"
  "Path to the emacsclient executable tmux should run."
  :type 'string
  :group 'termgrab)

(defvar termgrab-tmux-proc nil
  "The tmux server started by `termgrab-start-server'.")

(defvar termgrab-frame nil
  "The frame running in tmux, while the tmux server is running.

In tests, it's more usual to call `termgrab-root-window' and
`termgrab-minibuffer-window' to manipulate the windows or just
give the buffer to grab to `termgrab-grab-buffer-into' or
`termgrab-grab-buffer-to-string'.")

(defvar termgrab--tmux-socket nil
  "Full path to the socket used to communicate with tmux.")
(defvar termgrab--orig-server-name nil
  "Original value of `server-name'.

This is set by `termgrab-start-server' just before overriding
`server-name'.")

(defconst termgrab--server-output-buffer-name " *termgrab-server-output*"
  "Name of the buffer to which the tmux server output is sent.")

(defun termgrab-live-p (&optional proc)
  "Return non-nil if the tmux process started by termgrab is live.

PROC defaults to `termgrab-tmux-proc'."
  (let ((proc (or proc termgrab-tmux-proc)))
    (and proc (process-live-p proc))))

(defun termgrab-require-server (&optional proc)
  "Fail unless the tmux process started by termgrab is live.

PROC defaults to `termgrab-tmux-proc'."
  (let ((proc (or proc termgrab-tmux-proc)))
    (unless (termgrab-live-p proc)
      (error "Missing termgrab tmux process proc:%s" (when proc (process-status proc))))))

(defun termgrab--tmux-cmd ()
  "Command and arguments used to start the tmux server and clients."
  (list termgrab-tmux-exe "-S" termgrab--tmux-socket "-f" "/dev/null"))

(defun termgrab-restart-server ()
  "Stop and restart the tmux server, and optionally the Emacs server."
  (interactive)
  (termgrab-stop-server)
  (termgrab-start-server))

(defun termgrab-start-server ()
  "Start the tmux server required by termgrab, and optionally the Emacs server.

After this function has run `termgrab-frame' is set to the frame
that's running under tmux. If the server is already running, this
function resets some of its attribute, in case they've been
changed by previous tests.

It's a good idea to call this function before each test, to make
sure the server is available and in a reasonable state."
  (interactive)
  ;; If we need to start a server to communicate with this Emacs
  ;; process, make sure the server name is unique for tests, so as not
  ;; to interfere with any running Emacs server.
  (unless (and server-process (process-live-p server-process))
    (unless termgrab--orig-server-name
      (setq termgrab--orig-server-name server-name))
    (setq server-name (format "termgrab-%s" (emacs-pid)))
    (server-start nil 'inhibit-prompt))

  (unless (termgrab-live-p)
    (let ((old-frame (selected-frame))
          proc new-frame new-frame-func)

      ;; Keep the tmux socket into the same directory as the server
      ;; socket, so they have the same access limits.
      (server-ensure-safe-dir server-socket-dir)
      (setq termgrab--tmux-socket
            (expand-file-name (format "tmux-termgrab-%s" (emacs-pid))
                              server-socket-dir))
      (when (file-exists-p termgrab--tmux-socket)
        (delete-file termgrab--tmux-socket))

      (add-hook 'kill-emacs-hook #'termgrab-stop-server)
      (unwind-protect
          (progn
            (setq proc (make-process :name "*termgrab-server*"
                                     :buffer termgrab--server-output-buffer-name
                                     :connection-type 'pty
                                     :command (append (termgrab--tmux-cmd) '("-D"))))
            (set-process-query-on-exit-flag proc nil)
            (termgrab--wait-for 5 "server failed to start"
             (lambda () (file-exists-p termgrab--tmux-socket)))
            (setq new-frame-func (lambda ()
                                   (setq new-frame (selected-frame))))
            (add-hook 'server-after-make-frame-hook new-frame-func)
            (unwind-protect
                (progn
                  (apply
                   #'termgrab--tmux
                   proc nil "new-session" "-d" "-s" "grab" "-x" "80" "-y" "20"
                   termgrab-emacsclient-exe "-nw" "-c"
                   (if server-use-tcp
                       `("-f" ,(expand-file-name server-name server-auth-dir))
                     `("-s" ,(expand-file-name server-name server-socket-dir))))
                  (termgrab--wait-for 5 "emacsclient failed to connect" (lambda () new-frame)))
              (remove-hook 'server-after-make-frame-hook new-frame-func)

              ;; The new frame shouldn't be selected when running
              ;; interactively. It'll be selected later by the tests.
              (select-frame old-frame))
            (setq termgrab-tmux-proc proc)
            (setq termgrab-frame new-frame)

            ;; Recover some space
            (with-selected-frame termgrab-frame
              (toggle-menu-bar-mode-from-frame -1))

            ;; Success. Don't kill process in the unwind section
            (setq proc nil))
        (when (and proc (termgrab-live-p proc))
          (kill-process proc)))))

  ;; Always start with a single window
  (delete-other-windows (car (window-list termgrab-frame))))

(defun termgrab-stop-server ()
  "Stop the tmux server, does nothing if the server is not running.

This is done automatically just before Emacs shuts down."
  (interactive)
  (when (string-prefix-p "termgrab-" server-name)
    ;; Unintuitively, this stops the server. server-force-delete
    ;; sounds more appropriate, but sometimes prompts.
    (server-start 'leave-dead 'inhibit-prompt))
  (when termgrab--orig-server-name
    (setq server-name termgrab--orig-server-name))
  (setq termgrab--orig-server-name nil)

  (when (buffer-live-p termgrab--server-output-buffer-name)
    (kill-buffer termgrab--server-output-buffer-name))

  (when termgrab-tmux-proc
    (when (process-live-p termgrab-tmux-proc)
      (kill-process termgrab-tmux-proc)))
  (setq termgrab-tmux-proc nil)
  (setq termgrab-frame nil)

  (when (and termgrab--tmux-socket (file-exists-p termgrab--tmux-socket))
    (delete-file termgrab--tmux-socket))
  (setq termgrab--tmux-socket nil))

(defun termgrab-setup-buffer (&optional buf)
  "Setup the termgrab frame to display BUF in its root window.

If BUF is nil, the current buffer is used instead."
  (termgrab-require-server)

  (delete-other-windows (car (window-list termgrab-frame)))
  (let ((win (frame-root-window termgrab-frame)))
    (set-window-buffer win (or buf (current-buffer)))))

(defun termgrab-root-window ()
  "Return the root window of the grabbed frame."
  (frame-root-window termgrab-frame))

(defun termgrab-minibuffer-window ()
  "Return the minibuffer window of the grabbed frame."
  (minibuffer-window termgrab-frame))

(defun termgrab-grab-buffer-into (buf output-buf)
  "Display BUF in the grabbed frame and grab it into OUTPUT-BUF.

When this function returns, OUTPUT-BUF contains the textual
representation of BUF as displayed in the root window of the
grabbed frame."
  (termgrab-setup-buffer buf)
  (termgrab-grab-window-into (termgrab-root-window) output-buf))

(defun termgrab-grab-window-into (win output-buf)
  "Grab WIN into output-buf.

WIN must be a window on the termgrab frame.

When this function returns, OUTPUT-BUF contains the textual
representation of the content of that window."
  (unless (eq (window-frame win) termgrab-frame)
    (error "Window is not part of the termgrab frame: %s" win))

  (termgrab-grab-frame-into output-buf)

  (with-current-buffer output-buf
    (save-excursion
      (pcase-let ((`(,left ,top ,right ,bottom) (window-body-edges win)))

        (goto-char (point-min))
        (while (progn
                 (move-to-column right)
                 (delete-region (point) (pos-eol))
                 (= (forward-line 1) 0)))

        (when (> left 0)
          (goto-char (point-min))
          (while (progn
                   (move-to-column left)
                   (when (and noninteractive
                              (not (char-before ?|)))
                     (error
                      (concat "Capturing a window to the right of another "
                              "doesn't work because of rendering errors in "
                              "batch mode. Either always split horizontally "
                              "or run tests in non-batch mode.")))
                   (delete-region (pos-bol) (point))
                   (= (forward-line 1) 0))))

        (goto-char (point-min))
        (forward-line bottom)
        (delete-region (point) (point-max))

        (when (> top 0)
          (goto-char (point-min))
          (forward-line top)
          (delete-region (point-min) (point)))))))

(defun termgrab-grab-buffer-to-string (buf)
  "Grab BUF into a string.

See `termgrab-grab-buffer-into' for more details."
  (with-temp-buffer
    (termgrab-grab-buffer-into buf (current-buffer))
    (buffer-string)))

(defun termgrab-grab-window-to-string (win)
  "Grab WIN into a string.

See `termgrab-grab-window-into' for more details."
  (with-temp-buffer
    (termgrab-grab-window-into win (current-buffer))
    (buffer-string)))

(defun termgrab-grab-frame-to-string ()
  "Grab the frame into a string.

See `termgrab-grab-frame-into' for more details."
  (with-temp-buffer
    (termgrab-grab-frame-into (current-buffer))
    (buffer-string)))

(defun termgrab-grab-frame-into (buffer)
  "Grab the frame running under tmux into BUFFER.

This includes all windows and decorations. Unless that's what you
want to test, it's usually better to call `termgrab-grab-buffer'
or `termgrab-grab-win', which just return the window body."
  (with-selected-frame termgrab-frame
    (with-selected-window (car (window-list termgrab-frame))
      (redraw-frame termgrab-frame)
      (redisplay 'force)))

  (with-current-buffer buffer
    (delete-region (point-min) (point-max))
    (termgrab--tmux termgrab-tmux-proc buffer
                    "capture-pane" "-t" "grab:0" "-e" "-p")
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun termgrab--tmux (proc buffer &rest commands)
  "Execute the tmux client commands COMMANDS.

Communicates with PROC, usually `termgrab-tmux-proc' and stores
stdout into BUFFER."
  (termgrab-require-server proc)
  (with-temp-buffer
    (let ((tmux-cmd (append (termgrab--tmux-cmd) '("-N" "--") commands))
          proc)
      (insert "cmd: ")
      (insert (mapconcat #'shell-quote-argument tmux-cmd " "))
      (insert "\n")
      (setq proc (make-process :name "*termgrab-client*"
                               :buffer (or buffer (current-buffer))
                               :stderr (current-buffer)
                               :sentinel #'ignore
                               :command tmux-cmd))
      (while (process-live-p proc)
        (accept-process-output))
      (when (not (zerop (process-exit-status proc)))
        (error "Command failed: tmux %s [%s] %s"
               (string-join commands " ")
               (process-exit-status proc)
               (buffer-substring-no-properties (point-min) (point-max)))))))


(defun termgrab--wait-for (timeout error-message predicate)
  "Wait for up to TIMEOUT seconds for PREDICATE to become non-nil.

Fails with ERROR-MESSAGE if it times out."
  (let ((start (current-time)))
    (while (and (< (time-to-seconds (time-subtract (current-time) start)) timeout)
                (not (funcall predicate)))
      (accept-process-output nil 0 500)))
  (unless (funcall predicate)
    (error (concat "termgrab:timeout: " error-message))))

(provide 'termgrab)

;;; termgrab.el ends here
