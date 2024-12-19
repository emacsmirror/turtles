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

(defvar turtles-instance-alist nil
  "Alist of symbol to `turtles-instance' definitions.")

(defvar turtles--upstream nil
  "Connection to the Emacs instance that started this one.

Accessed using `turtles-upstream'")

(defvar turtles--this-instance nil
  "The ID of the instance that identifies the current process.
Accessed using `turtles-this-instance'.")

(defvar turtles-read-instance-history nil)

(defconst turtles--file-name (or (when load-file-name
                                 (expand-file-name load-file-name default-directory))
                               (buffer-file-name)))

(cl-defstruct (turtles-instance
               (:constructor turtles--make-instance)
               (:copier nil))
  "An Emacs sub-process maintained by Turtles."
  (id nil :read-only t :documentation "Instance name, as a symbol.")
  (doc nil :read-only t :documentation "Description.")
  (conn nil :documentation "A turtles-io-conn connected with the instance, if live.")
  (width 80 :read-only t :documentation "Terminal width, in characters.")
  (height 24 :read-only t :documentation "Terminal height, in characters.")
  (setup nil :read-only t :documentation "Expression to execute before every test.")
  (term-buf nil :documentation "Buffer running this instance, if live."))

(defun turtles-get-instance (id)
  "Get an instance from its ID."
  (alist-get id turtles-instance-alist))

(defun turtles-instance-shortdoc (inst)
  "Return the first line of the documentation of INST.

Return the empty string if there is no documentation."
  (if-let ((doc (turtles-instance-doc inst)))
      (car (string-split doc"\n"))
    ""))

(defsubst turtles-instance-term-proc (inst)
  "Returns the process running in the term window.

This is the Emacs subprocess."
  (when-let ((b (turtles-instance-term-buf inst)))
    (get-buffer-process b)))

(defun turtles-instance-live-p (inst)
  "Return non-nil if INST is a live instance."
  (and inst
       (turtles-instance-p inst)
       (turtles-io-conn-live-p (turtles-instance-conn inst))
       (buffer-live-p (turtles-instance-term-buf inst))
       (process-live-p
        (get-buffer-process
         (turtles-instance-term-buf inst)))
       t))

(cl-defmacro turtles-definstance (id (&key (width 80) (height 24))
                                     doc &rest setup)
  "Define an instance with the given ID.

DOC is a documentation string for that instance.

WIDTH and HEIGHT are the terminal dimensions.

SETUP is code to run on the instance before every test."
  (declare (indent 2) (doc-string 3))
  `(setf (alist-get ',id turtles-instance-alist)
         (turtles--make-instance
          :id ',id
          :doc ,doc
          :width ,width
          :height ,height
          :setup '(progn ,@setup))))

(turtles-definstance default (:width 80 :height 20)
  "Emacs instance to run tests on.

This is the instance used by `ert-test' when no instance is
given."
  (when (eval-when-compile (>= emacs-major-version 29))
    (clear-minibuffer-message))
  (menu-bar-mode -1))

(defun turtles-this-instance ()
  "ID of the instance for which the current Emacs process was started.

If the Emacs process was started by Turtles, this contains the instance ID."
  turtles--this-instance)

(defun turtles-upstream ()
  "Return the connection to the upstream Emacs instance.

If the current Emacs process was started by Turtles, this returns
a connection to the Emacs process that started this on."
  turtles--upstream)

(defun turtles-start-server ()
  "Start a server listening on the turtles socket.

This is the server all instances communicate with.

Does nothing if the server is already live."
  (unless (turtles-io-server-live-p turtles--server)
    (server-ensure-safe-dir server-socket-dir)
    (setq turtles--server
          (turtles-io-server
           (expand-file-name (format "turtles-%s" (emacs-pid))
                             server-socket-dir)
           `((register . ,(lambda (conn id _method instance-id)
                            (let ((ret (when-let ((inst (turtles-get-instance instance-id)))
                                         (setf (turtles-instance-conn inst) conn)

                                         t)))
                            (when id
                              (turtles-io--send conn `(:id ,id :result ,ret))))))
             (grab . ,(turtles-io-method-handler (instance-id)
                        (let ((inst (turtles-get-instance instance-id)))
                          ;; Wait until all output from the other
                          ;; Emacs instance have been processed, as
                          ;; it's likely in the middle of a redisplay.
                          (turtles-instance-let-term-settle inst)
                          (with-current-buffer (turtles-instance-term-buf inst)
                            (buffer-substring term-home-marker (point-max))))))
             (message . ,(lambda (_conn _id _method msg)
                           (message msg))))))))

(defun turtles-shutdown ()
  (interactive)
  (mapc (lambda (cell)
          (turtles-stop-instance (cdr cell)))
        turtles-instance-alist)

  (when-let ((s turtles--server))
    (when (and s (turtles-io-server-p s))
    (when-let ((proc (turtles-io-server-proc s)))
      (delete-process proc))
    (when-let ((f (turtles-io-server-socket s)))
      (ignore-errors (delete-file f)))
    (setq turtles--server nil))))

(defun turtles-start-instance (inst)
  "Start instance INST.

Does nothing if the instance is already running."
  (interactive (list
                (turtles-read-instance
                 "Instance to start: "
                 (lambda (inst)
                   (not (turtles-instance-live-p inst))))))
  (turtles-start-server)
  (unless (turtles-instance-live-p inst)
    (turtles-stop-instance inst) ; cleanup
    (setf (turtles-instance-term-buf inst)
          (if (buffer-live-p (turtles-instance-term-buf inst))
              (turtles-instance-term-buf inst)
            (generate-new-buffer (format " *term-%s*" (turtles-instance-id inst)))))
    (with-current-buffer (turtles-instance-term-buf inst)
      (term-mode)
      (setq-local face-remapping-alist turtles-term-face-remapping-alist)
      (setq-local term-width (turtles-instance-width inst))
      (setq-local term-height (turtles-instance-height inst))

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
                                     (turtles--launch
                                      ,(turtles-io-server-socket turtles--server)
                                      ',(turtles-instance-id inst)
                                      (lambda () ,(turtles-instance-setup inst))))))))
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
                           (lambda () (turtles-instance-conn inst)))
      (message "Turtles started %s: %s" (turtles-instance-id inst)
               (turtles-instance-shortdoc inst))))

  (with-current-buffer (turtles-instance-term-buf inst)
    (let ((w (turtles-instance-width inst))
          (h (turtles-instance-height inst)))
      (unless (and (= term-width w) (= term-height h))
        (set-process-window-size (get-buffer-process (turtles-instance-term-buf inst)) h w)
        (term-reset-size h w)
        (turtles-instance-let-term-settle inst)))))

(defun turtles-stop-instance (inst)
  (interactive
   (list
    (turtles-read-instance
     "Instance to stop: " #'turtles-instance-live-p)))
  (when-let ((c (turtles-instance-conn inst)))
    (when (turtles-io-conn-live-p c)
      (turtles-io-notify c 'exit))
    (while (accept-process-output))
    (when-let ((p (turtles-io-conn-proc c)))
      (when (process-live-p p)
        (delete-process p)))
    (setf (turtles-instance-conn inst) nil))
  (when-let ((b (turtles-instance-term-buf inst))
             (p (get-buffer-process b)))
    (when (process-live-p p)
      (delete-process p))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer b))))

(defun turtles-read-instance (prompt predicate)
  (turtles-get-instance
   (intern
    (completing-read
     prompt
     turtles-instance-alist
     (lambda (cell)
       (funcall predicate (cdr cell)))
     'require-match nil 'turtles-read-instance-history))))

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

(defun turtles--launch (socket instance-id setup-func)
  (interactive "F")
  (setq turtles--this-instance instance-id)
  (advice-add 'message :after #'turtles--send-message-up)
  (setq turtles--upstream
        (turtles-io-connect
         socket
         `((eval
            . ,(turtles-io-method-handler (expr)
                 (turtles--with-incremented-var turtles--should-send-messages-up
                   (eval expr))))
           (ert-test
            . ,(lambda (conn id _method expr)
                 (catch 'turtles-return
                   (condition-case err
                       (turtles--with-incremented-var turtles--should-send-messages-up
                         (when setup-func
                           (funcall setup-func)))
                     ((error t)
                      (turtles-io--send conn `(:id ,id :error ,err))
                      (throw 'turtles-return nil)))
                   (condition-case-unless-debug err
                       (turtles--with-incremented-var turtles--should-send-messages-up
                         (turtles-io--send conn `(:id ,id :result ,(eval expr))))
                     ((error t) (turtles-io--send conn `(:id ,id :error ,err)))))))
           (last-messages . ,(turtles-io-method-handler (count)
                               (with-current-buffer (messages-buffer)
                                 (save-excursion
                                   (goto-char (point-max))
                                   (forward-line (- (or count 5)))
                                   (buffer-substring-no-properties (point) (point-max))))))
           (exit
            . ,(lambda (_conn _id _method _params)
                 (kill-emacs nil))))))
  (turtles-io-notify turtles--upstream 'register instance-id))

(defun turtles--send-message-up (msg &rest args)
  "Send a message to the server."
  (when (and (turtles-upstream)
             (> turtles--should-send-messages-up 0)
             (not (> turtles--sending-messages-up 0)))
    (turtles--with-incremented-var turtles--sending-messages-up
      (turtles-io-notify (turtles-upstream) 'message
                         (concat (format "[%s] " (turtles-this-instance))
                                 (apply #'format msg args))))))

(defun turtles-new-client-frame (inst)
  "Ask the instance INST to create a new frame.

This opens a new frame on the Emacs instance run by turtles on a
window system, which is convenient for debugging.

The frame that is created is on the same display as the current
frame, which only makes sense for graphical displays."
  (interactive
   (list
    (turtles-read-instance
     "Instance: " #'turtles-instance-live-p)))
  (unless (turtles-instance-live-p inst)
    (error "Instance not running: %s" (turtles-instance-id inst)))
  (let ((params (frame-parameters)))
    (unless (alist-get 'window-system params)
      (error "No window system"))
    (message "New client frame for %s: %s"
             (turtles-instance-id inst)
             (turtles-io-call-method
              (turtles-instance-conn inst) 'eval
              `(progn
                 (prin1-to-string
                  (make-frame
                   '((window-system . ,(alist-get 'window-system params))
                     (display . ,(alist-get 'display params))))))))))

(defun turtles-instance-let-term-settle (inst)
  "Wait until the Emacs process of INST is done updating the buffer."
  (when-let ((p (turtles-instance-term-proc inst)))
    (when (accept-process-output p 0.05)
      (accept-process-output p 0))))

(provide 'turtles-instance)

;;; turtles-instance.el ends here
