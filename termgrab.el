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

(require 'ansi-color)
(require 'cl-lib)
(require 'faces)
(require 'pcase)
(require 'server)

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

(defvar-local termgrab-source-window nil
  "The termgrab frame window the current buffer was grabbed from.

This is local variable set in a grab buffer filled by
`termgrab-grab-window-into' or `termgrab-grab-buffer-into'.")

(defvar-local termgrab-source-buffer nil
  "The buffer the current buffer was grabbed from.

This is local variable set in a grab buffer filled by
`termgrab-grab-window-into' or `termgrab-grab-buffer-into'.")

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
                   proc nil
                   "new-session" "-d" "-s" "grab" "-x" "80" "-y" "20"
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

(defun termgrab-grab-buffer-into (buf output-buf &optional grab-faces)
  "Display BUF in the grabbed frame and grab it into OUTPUT-BUF.

When this function returns, OUTPUT-BUF contains the textual
representation of BUF as displayed in the root window of the
grabbed frame.

This function uses `termgrab-grab-window-into' after setting up
the buffer. See the documentation of that function for details on
the buffer content and the effect of GRAB-FACES."
  (termgrab-setup-buffer buf)
  (termgrab-grab-window-into (termgrab-root-window) output-buf grab-faces))

(defun termgrab-grab-window-into (win output-buf &optional grab-faces)
  "Grab WIN into output-buf.

WIN must be a window on the termgrab frame.

When this function returns, OUTPUT-BUF contains the textual
representation of the content of that window. The point, mark and
region are also set to corresponding positions in OUTPUT-BUF, if
possible.

If GRAB-FACES is empty, the colors are copied as
\\='font-lock-face text properties, with as much fidelity as the
tmux terminal behind `termgrab-frame' allows.

If GRAB-FACES is not empty, the faces on that list - and only
these faces - are recovered into \\='face text properties. Note
that in such case, no other face or color information is grabbed,
so any other face not in GRAB-FACE are absent."
  (unless (eq (window-frame win) termgrab-frame)
    (error "Window is not part of the termgrab frame: %s" win))

  (termgrab-grab-frame-into output-buf grab-faces)
  (with-current-buffer output-buf
    (setq termgrab-source-window win)
    (setq termgrab-source-buffer (window-buffer win))
    (termgrab--clip-in-frame-grab win)

    (let ((point-pos (termgrab-pos-in-window-grab (window-point win)))
          (mark-pos (termgrab-pos-in-window-grab
                     (with-selected-window win (mark)) 'range)))
      (when point-pos
        (goto-char point-pos))
      (when mark-pos
        (push-mark mark-pos 'nomsg nil))

      (when (and point-pos
                 mark-pos
                 (with-selected-window win
                   (region-active-p)))
        (activate-mark)))))

(defun termgrab-pos-in-window-grab (pos-in-source-buf &optional range)
  "Convert a position in the source buffer to the current buffer.

For this to work, the current buffer must be a grab buffer
created by `termgrab-grab-window-into' or
`termgrab-grab-buffer-into' and neither its content nor the
source buffer or source window must have changed since the grab.

POS-IN-SOURCE-BUF should be a position in the source buffer. It
might be nil, in which case this function returns nil.

When RANGE is non-nil, if the position is before window start,
set it at (point-min), if it is after window end, set it
at (point-max). This is appropriate when highlighting range
boundaries.

Return a position in the current buffer. If the point does not
appear in the grab, return nil."
  (unless termgrab-source-window
    (error "Current buffer does not contain a window grab"))

  (cond
   ((null pos-in-source-buf) nil)
   ((and range (<= pos-in-source-buf
                   (window-start termgrab-source-window)))
    (point-min))
   ((and range (>= pos-in-source-buf
                   (window-end termgrab-source-window)))
    (point-max))
   (t (pcase-let ((`(,x . ,y) (window-absolute-pixel-position
                               pos-in-source-buf termgrab-source-window)))
        (when (and x y)
          (save-excursion
            (goto-char (point-min))
            (forward-line y)
            (move-to-column x)
            (point)))))))

(defun termgrab--clip-in-frame-grab (win)
  "Clip the frame grab in the current buffer to the body of WIN."
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
        (delete-region (point-min) (point))))))

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

(defun termgrab-grab-frame-into (buffer &optional grab-faces)
  "Grab the frame running under tmux into BUFFER.

This includes all windows and decorations. Unless that's what you
want to test, it's usually better to call `termgrab-grab-buffer'
or `termgrab-grab-win', which just return the window body.

If GRAB-FACES is empty, the colors are copied as
\\='font-lock-face text properties, with as much fidelity as the
tmux terminal behind `termgrab-frame' allows.

If GRAB-FACES is not empty, the faces on that list - and only
these faces - are recovered into \\='face text properties. Note
that in such case, no other face or color information is grabbed,
so any other face not in GRAB-FACE are absent."
  (pcase-let ((`(,grab-face-alist . ,cookies)
               (termgrab--setup-grab-faces grab-faces)))
    (unwind-protect
        (progn
          (with-selected-frame termgrab-frame
            (with-selected-window (car (window-list termgrab-frame))
              (redraw-frame termgrab-frame)
              (redisplay 'force)))

          (with-current-buffer buffer
            (delete-region (point-min) (point-max))
            (termgrab--tmux termgrab-tmux-proc buffer
                            "capture-pane" "-t" "grab:0" "-e" "-p")

            (if grab-faces
                ;; Set 'face properties for the faces in grab-faces,
                ;; detected from colors setup by
                ;; termgrab--setup-grab-faces.
                (progn
                  (let ((ansi-color-apply-face-function #'ansi-color-apply-text-property-face))
                    (ansi-color-apply-on-region (point-min) (point-max)))
                  (termgrab--faces-from-color grab-face-alist))

              ;; Grab colors as close to the original as the terminas allows.
              ;;
              ;; This uses overlays, because
              ;; foreground/background-color-at-point don't work with
              ;; font lock text properties in batch mode.
              (let ((ansi-color-apply-face-function #'ansi-color-apply-overlay-face))
                (ansi-color-apply-on-region (point-min) (point-max))))))
      (termgrab--teardown-grab-faces cookies))))

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

(defun termgrab--setup-grab-faces (grab-faces)
  "Prepare buffer faces for grabbing GRAB-FACES.

This function modifies the faces in all buffers of
`termgrab-frame' so that they can be detected from color by
`termgrab--faces-from-color'.

The color changes are reverted by `termgrab--teardown-grab-faces'
or the grabbed buffers will look very ugly.

Return a (cons grab-face-alist cookies) with grab-face-alist the
alist to pass to `termgrab--faces-from-color' and cookies to pass
to `termgrab--teardown-grab-faces'."
  (when grab-faces
    (let (grab-face-alist cookies remapping)

      ;; This algorithm limits itself to normal (non-bright) ansi
      ;; colors, excluding black and white, for simplicity. That gives
      ;; us 6 color - 36 combination of background and foreground - to
      ;; work with. That should be enough for most reasonable tests,
      ;; but if not, it could be extended to use more colors, as we
      ;; normally have access to 256 colors.
      (when (> (length grab-faces) 36)
        (error "Too many faces to highlight"))

      (dolist (face grab-faces)
        (let* ((idx (length remapping))
               (bg (face-background
                    (aref ansi-color-normal-colors-vector
                          (1+ (% idx 6)))))
               (fg (face-foreground
                    (aref ansi-color-bright-colors-vector
                          (1+ (/ idx 6))))))
          (push (cons face `(:background ,bg :foreground ,fg)) remapping)))

      (setq grab-face-alist (cl-copy-list remapping))

      ;; Set *all* other faces to white-on-black so there won't be any
      ;; confusion.
      (let* ((white-fg (face-foreground 'ansi-color-white))
             (black-bg (face-background 'ansi-color-black))
             (white-on-black
              `(:foreground ,white-fg :background ,black-bg)))
        (dolist (face (face-list))
          (unless (memq face grab-faces)
            (push (cons face white-on-black) remapping))))

      (dolist (buf (let ((bufs (list)))
                     (dolist (win (window-list termgrab-frame))
                       (when-let ((buf (window-buffer win)))
                         (unless (memq buf bufs)
                           (push buf bufs))))

                     bufs))
        (with-current-buffer buf
          (push (cons buf (buffer-local-value 'face-remapping-alist buf))
                cookies)
          (setq-local face-remapping-alist remapping)))

      (cons grab-face-alist cookies))))

(defun termgrab--teardown-grab-faces (cookies)
  "Revert buffer colors modified by `termgrab--setup-grab-faces'.

COOKIES is one of the return values of
`termgrab--setup-grab-faces'."
  (pcase-dolist (`(,buf . ,remapping) cookies)
    (with-current-buffer buf
      (setq-local face-remapping-alist remapping))))

(defun termgrab--faces-from-color (face-alist)
  "Recognize faces from FACE-ALIST in current buffer.

This function replaces the font-lock-face color properties set by
ansi-color with face properties from FACE-ALIST.

FACE-ALIST must be an alist of face symbol to face spec, as
returned by termgrab--setup-grab-faces. The colors in this alist
are mapped back to the symbols.

When this function returns, the buffer content should look like
the original content, but with only the faces from FACE-ALIST
set."
  (let ((reverse-face-alist
         (mapcar (lambda (cell)
                   (cons (termgrab--color-values (cdr cell)) (car cell)))
                 face-alist))
        current-face range-start next)
    (save-excursion
      (goto-char (point-min))
      (setq next (point-min))
      (while
          (progn
            (goto-char next)
            (when-let* ((spec (get-text-property (point) 'font-lock-face))
                        (col (termgrab--color-values spec))
                        (face (alist-get col reverse-face-alist nil nil #'equal)))
              (setq current-face face)
              (setq range-start (point)))
            (setq next (next-property-change (point)))

            (let ((next (or next (point-max))))
              (when (> next (point))
                (remove-text-properties (point) next '(font-lock-face nil)))
              (when current-face
                (add-text-properties range-start next `(face ,current-face))
                (setq range-start nil)
                (setq current-face nil)))

            next)))))

(defun termgrab--color-values (spec)
  "Extract fg/bg color values from SPEC.

The color values are constrained to colors inside of
`termgrab-frame'.

SPEC might be a face symbol, a face attribute list or a list of
face attribute lists.

Returns a (cons fg bg) with fg and bg a list of 3 integers (red
green blue) between 0 and 65535."
  (cons (color-values (or (termgrab--face-attr spec :foreground)
                          "unspecified-fg")
                      termgrab-frame)
        (color-values (or (termgrab--face-attr spec :background)
                          "unspecified-bg")
                      termgrab-frame)))

(defun termgrab--face-attr (spec attr)
  "Extract ATTR from SPEC.

SPEC might be a face symbol, a face attribute list or a list of
face attribute lists."
  (cond
   ((symbolp spec)
    (face-attribute-specified-or
     (face-attribute spec attr nil t)
     nil))
   ((consp spec)
    (or (cadr (memq attr spec))
        (car (delq nil
                   (mapcar
                    (lambda (s) (when (consp s) (termgrab--face-attr s attr)))
                    spec)))))))

(defsubst termgrab-mark-text-with-face (face marker &optional closing-marker)
  "Put section of text marked with FACE within MARKERS.

MARKER should either be a string made up of two markers of the
same length, such as \"[]\" or the opening marker string, with
the closing marker defined by CLOSING-MARKER.

This function is a thin wrapper around
`termgrab-mark-text-with-face'. See the documentatin of that
function for details."
  (termgrab-mark-text-with-faces
   `((,face ,marker . ,(when closing-marker (cons closing-marker nil))))))

(defun termgrab-mark-text-with-faces (face-marker-alist)
  "Put section of text marked with specific faces with text markers.

FACE-MARKER-ALIST should be an alist of (face markers),
with face a face symbol to detect and marker.

The idea behind this function is to make face properties visible
in the text, to make easier to test buffer content with faces by
comparing two strings.

markers should be, either:

- a string made up an opening and closing substring of the same
  length or two strings. For example, \"()\" \"[]\" \"<<>>\"
  \"/**/\".

- two strings, the opening and closing substrings.
  For example: (\"s[\" \"]\")

This function is meant to highlight faces setup by termgrab when
asked to grab faces. It won't work in the general case."
  (save-excursion
    (let ((next (point-min))
          (closing nil))
      (while
          (progn
            (goto-char next)
            (when-let* ((face (get-text-property (point) 'face))
                        (markers (alist-get face face-marker-alist)))
              (pcase-let ((`(,op . ,close) (termgrab--split-markers markers)))
                (insert op)
                (setq closing close)))
            (setq next (next-property-change (point)))

            (when closing
              (goto-char (or next (point-max)))
              (insert closing)
              (setq closing nil))

            next)))))

(defun termgrab--split-markers (markers)
  "Return an opening and closing marker.

MARKERS must be either a string, to be split into two strings of
the same length or a list of two elements.

The return value is a (cons opening closing) containing two
strings"
  (cond
   ((and (consp markers) (length= markers 1))
    (termgrab--split-markers (car markers)))
   ((and (consp markers) (length= markers 2))
    (cons (nth 0 markers) (nth 1 markers)))
   ((stringp markers)
    (let ((mid (/ (length markers) 2)))
      (cons (substring markers 0 mid) (substring markers mid))))
   (t (error "Unsupported markers: %s" markers))))

(defun termgrab-mark-region (marker &optional closing-marker)
  "Surround the active region with markers.

This function does nothing if the region is inactive.

If only MARKER is specified, it must be a string composed of two
strings of the same size that will be used as opening and closing
marker, such as \"[]\" or \"/**/\".

If both MARKER and CLOSING-MARKER are specified, MARKER is used
as opening marker and CLOSING-MARKER as closing."
  (when (region-active-p)
    (pcase-let ((`(,opening . ,closing)
                 (termgrab--split-markers
                  (if closing-marker
                      (list marker closing-marker)
                    marker))))
      (let ((beg (min (point-marker) (mark-marker)))
            (end (max (point-marker) (mark-marker))))
        (save-excursion
          (goto-char end)
          (insert closing)
          (goto-char beg)
          (insert opening))))))

(provide 'termgrab)

;;; termgrab.el ends here
