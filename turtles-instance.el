;;; turtles-instance.el --- Manage turtles subprocesses -*- lexical-binding: t -*-

;; Copyright (C) 2024 Stephane Zermatten

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
;; This package manages Emacs sub-processes started by turtles.
;;

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'server)
(require 'subr-x) ;; when-let
(require 'term)
(require 'turtles-io)

(defvar term-home-marker) ;; declared in term.el
(defvar term-width) ;; declared in term.el
(defvar term-height) ;; declared in term.el

(defconst turtles-term-face-remapping-alist
  '((term :foreground "#ffffff" :background "#000000")
    (term-color-black :foreground "#000000" :background "#000000")
    (term-color-red :foreground "#ff0000" :background "#ff0000")
    (term-color-green :foreground "#00ff00" :background "#00ff00")
    (term-color-blue :foreground "#0000ff" :background "#0000ff")
    (term-color-yellow :foreground "#ffff00" :background "#ffff0")
    (term-color-magenta :foreground "#ff00ff" :background "#ff00ff")
    (term-color-cyan :foreground "#00ffff" :background "#00ffff")
    (term-color-white :foreground "#ffffff" :background "#fffff"))
  "Hardcoded color faces for term-mode, for consistency.")

(defvar turtles--should-send-messages-up 0
  "When this is > 0, send messages to the server.")

(defvar turtles--sending-messages-up 0
  "Set to > 0 while processing code to send messages to the server.

This is used in `tultles--send-messages-up' to avoid entering
into a loop, sending messages while sending messages.")

(defvar turtles--server nil)
(defvar turtles--conn nil)
(defvar turtles--file-name (or (when load-file-name
                                 (expand-file-name load-file-name default-directory))
                               (buffer-file-name)))
(defconst turtles-buffer-name " *turtles-term*")

(defun turtles-client-p ()
  (and turtles--conn (not turtles--server)))

(defun turtles-start ()
  (interactive)
  (unless (turtles-io-server-live-p turtles--server)
    (server-ensure-safe-dir server-socket-dir)
    (setq turtles--server
          (turtles-io-server
           (expand-file-name (format "turtles-%s" (emacs-pid))
                             server-socket-dir)
           `((register . ,(lambda (conn id _method _params)
                            (setq turtles--conn conn)
                            (when id
                              (turtles-io--send conn `(:id ,id :result nil)))))
             (grab . ,(turtles-io-method-handler (_ignored)
                        (with-current-buffer (get-buffer turtles-buffer-name)
                          ;; Wait until all output from the other
                          ;; Emacs instance have been processed, as
                          ;; it's likely in the middle of a redisplay.
                          (while (accept-process-output
                                  (get-buffer-process (current-buffer)) 0.05))

                          (buffer-substring term-home-marker (point-max)))))
             (message . ,(lambda (_conn _id _method msg)
                           (message msg)))))))

  (unless (and (turtles-io-conn-live-p turtles--conn)
               (term-check-proc (get-buffer-create turtles-buffer-name)))
    (mapc (lambda (c) (turtles-io-call-method-async c 'exit nil nil))
          (turtles-io-server-connections turtles--server))
    (setq turtles--conn nil)
    (with-current-buffer (get-buffer-create turtles-buffer-name)
      (term-mode)
      (setq-local face-remapping-alist turtles-term-face-remapping-alist)
      (setq-local term-width 80)
      (setq-local term-height 20)

      (let ((cmdline `(,(expand-file-name invocation-name invocation-directory)
                       "-nw" "-Q")))
        (setq cmdline
              (append cmdline (turtles--dirs-from-load-path)))
        (setq cmdline
              (append cmdline
                      `("-eval" ,(prin1-to-string
                                  `(progn
                                     (setq load-prefer-newer t)
                                     (load ,turtles--file-name nil 'nomessage)
                                     (turtles--launch ,(turtles-io-server-socket
                                                        turtles--server)))))))
        (when (>= emacs-major-version 29)
          ;; COLORTERM=truecolor tells Emacs to use 24bit terminal
          ;; colors even though the termcap entry for eterm-color
          ;; only defines 256. That works, because term.el in
          ;; Emacs 29.1 and later support 24 bit colors.
          (setq cmdline `("env" "COLORTERM=truecolor" . ,cmdline)))
        (term-exec (current-buffer) "*turtles*" (car cmdline) nil (cdr cmdline)))
      (term-char-mode)
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (turtles-io-wait-for 5 "Turtles Emacs failed to connect"
                           (lambda () turtles--conn)))))

(defun turtles-stop ()
  (interactive)
  (when (turtles-io-server-live-p turtles--server)
    (mapc (lambda (c) (turtles-io-notify c 'exit))
          (turtles-io-server-connections turtles--server))
    (delete-process (turtles-io-server-proc turtles--server)))
  (setq turtles--server nil)
  (setq turtles--conn nil))

(defun turtles-fail-unless-live ()
  (unless (turtles-io-conn-live-p turtles--conn)
    (error "No Turtles! Call turtles-start")))

(defun turtles--dirs-from-load-path ()
  (let ((args nil))
    (dolist (path load-path)
      (push "-L" args)
      (push path args))
    (nreverse args)))

(defmacro turtles--with-incremented-var (var &rest body)
  "Increment VAR while BODY is running.

This is used instead of a let to account for the possibility of
more than one instance of BODY running at the same time, with
special cases like reading from the minibuffer."
  (declare (indent 1))
  `(progn
     (cl-incf ,var)
     (unwind-protect
         (progn ,@body)
       (cl-decf ,var))))

(defun turtles--launch (socket)
  (interactive "F")
  (advice-add 'message :after #'turtles--send-message-up)
  (setq turtles--conn
        (turtles-io-connect
         socket
         `((eval . ,(turtles-io-method-handler (expr)
                      (turtles--with-incremented-var turtles--should-send-messages-up
                        (eval expr))))
           (eval-ert . ,(lambda (conn id _method expr)
                          (condition-case-unless-debug err
                              (turtles-io--send
                               conn `(:id ,id :result ,(turtles--with-incremented-var
                                                           turtles--should-send-messages-up
                                                         (eval expr))))
                            (t (turtles-io--send conn `(:id ,id :error ,err))))))
           (exit . ,(lambda (_conn _id _method _params)
                      (kill-emacs nil))))))
  (turtles-io-notify turtles--conn 'register))

(defun turtles--send-message-up (msg &rest args)
  "Send a message to the server."
  (when (and turtles--conn
             (> turtles--should-send-messages-up 0)
             (not (> turtles--sending-messages-up 0)))
    (turtles--with-incremented-var turtles--sending-messages-up
      (turtles-io-notify turtles--conn 'message
                         (concat (format "[PID %s] " (emacs-pid))
                                 (apply #'format msg args))))))


(defun turtles-new-client-frame ()
  "Ask the client instance to create a new frame.

This opens a new frame on the Emacs instance run by turtles on a
window system, which is convenient for debugging.

The frame that is created is on the same display as the current
frame, which only makes sense for graphical displays."
  (interactive)
  (turtles-fail-unless-live)
  (let ((params (frame-parameters)))
    (unless (alist-get 'window-system params)
      (error "No window system"))
    (message "New client frame: %s"
             (turtles-io-call-method
              turtles--conn 'eval
              `(progn
                 (prin1-to-string
                  (make-frame
                   '((window-system . ,(alist-get 'window-system params))
                     (display . ,(alist-get 'display params))))))))))


(provide 'turtles-instance)

;;; turtles-instance.el ends here
