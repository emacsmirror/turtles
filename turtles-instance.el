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
(require 'pcase)
(require 'server)
(require 'subr-x) ;; when-let
(require 'turtles-io)
(require 'turtles-term)

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

(defvar turtles-instance-alist nil
  "Alist of symbol to `turtles-instance' definitions.")

(defun turtles-set-instance-option (sym val)
  "Set SYM to VAL on all instances.

This is meant to be passed to the :set option of customize
options."
  (set-default-toplevel-value sym val)
  (pcase-dolist (`(_ . ,inst) turtles-instance-alist)
    (when (turtles-instance-live-p inst)
      (ignore-errors
        (turtles-io-call-method
         (turtles-instance-conn inst)
         'eval
         `(set-default-toplevel-value ',sym ',val))))))

(defcustom turtles-send-messages-upstream t
  "Whether to forward messages from instances.

Turtles can ask instance it starts to send whatever messages is
registered to upstream. The messages appear in the message log
prefixed with the instance name in brackets.

If this is set to t instances send all messages.

If this is set to nil instances never send any messages.

Prefer setting it through customize, otherwise modifications only
take effect after instances are restarted."
  :group 'turtles
  :type '(radio (const :tag "Always" t)
                (const :tag "Never" nil))
  :set #'turtles-set-instance-option)

(defvar turtles--sending-messages-up 0
  "Set to > 0 while processing code to send messages to the server.

This is used in `tultles--send-messages-up' to avoid entering
into a loop, sending messages while sending messages.")

(defvar turtles--server nil)

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
  (term-buf nil :documentation "Buffer running this instance, if live.")
  (terminal 'term :documentation "Terminal type, \\='term or \\='eat."))

(defun turtles-get-instance (id)
  "Get an instance from its ID.

If ID is a `turtles-instance' already, just return in. This means
that turtles-get-instance can be used to make functions accept
either an instance or an instance id."
  (if (turtles-instance-p id)
      id
    (alist-get id turtles-instance-alist)))

(defun turtles-instance-shortdoc (inst)
  "Return the first line of the documentation of INST.

Return nil if there is no documentation."
  (when-let ((doc (turtles-instance-doc inst)))
      (car (string-split doc"\n"))))

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

(cl-defun turtles-instance-eval (inst-or-id expr &key timeout)
  "Evaluate EXPR on the instance INST-OR-ID.

INST-OR-ID can be a `turtles-instance' or an instance id.
EXPR must be a quoted elisp expression.

The result of evaluating EXPR on the given instance is returned.

If set, the key argument TIMEOUT specifies how long to wait for
the instance to evaluate the expression, in seconds.

This function does not start the instance, so if you're not sure
the instance is up, you might want to use
`turtles-start-instance' first.

For example:

  (turtles-instance-eval
     (turtles-start-instance \\='default)
     \\='(do-something))"
  (turtles-io-call-method
   (turtles-instance-conn (turtles-get-instance inst-or-id))
   'eval expr :timeout timeout))

(cl-defmacro turtles-definstance
    (id (&key (width 80) (height 24) (terminal 'term))
        doc &rest setup)
  "Define an instance with the given ID.

DOC is a documentation string for that instance.

WIDTH and HEIGHT are the terminal dimensions.

TERMINAL is the terminal type to use for this instance, term or
eat, term by default.

SETUP is code to run on the instance before every test."
  (declare (indent 2) (doc-string 3))
  `(setf (alist-get ',id turtles-instance-alist)
         (turtles--make-instance
          :id ',id
          :doc ,doc
          :width ,width
          :height ,height
          :terminal ',terminal
          :setup '(progn ,@setup))))

(turtles-definstance default (:width 80 :height 20)
  "Emacs instance to run tests on.

This is the instance used by `ert-test' when no instance is
given."
  (when (eval-when-compile (>= emacs-major-version 29))
    (clear-minibuffer-message))
  (menu-bar-mode -1))

(turtles-definstance eat (:terminal eat :width 80 :height 24)
  "Emacs instance in an eat terminal."
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
                          (turtles--let-term-settle inst)
                          (with-current-buffer (turtles-instance-term-buf inst)
                            (turtles--term-screen-string
                             (turtles-instance-terminal inst))))))
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

(defun turtles-start-instance (inst-or-id)
  "Start instance INST-OR-ID.

INST-OR-ID can be a `turtles-instance' or instance id.

Returns the `turtles-instance'.

Does nothing if the instance is already running."
  (interactive (list
                (turtles-read-instance
                 "Instance to start: "
                 (lambda (inst)
                   (not (turtles-instance-live-p inst))))))
  (let ((inst (turtles-get-instance inst-or-id)))
    (turtles-start-server)

    (unless (turtles-instance-live-p inst)
      (turtles-stop-instance inst) ; cleanup

      (pcase (turtles-instance-terminal inst)
        ('term (require 'turtles-term-term))
        ('eat (require 'turtles-term-eat)))

      (setf (turtles-instance-term-buf inst)
            (if (buffer-live-p (turtles-instance-term-buf inst))
                (turtles-instance-term-buf inst)
              (generate-new-buffer (format " *term-%s*" (turtles-instance-id inst)))))
      (with-current-buffer (turtles-instance-term-buf inst)
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
                                       (setq turtles-send-messages-upstream ',turtles-send-messages-upstream)
                                       (turtles--launch
                                        ,(turtles-io-server-socket turtles--server)
                                        ',(turtles-instance-id inst)
                                        (lambda () ,(turtles-instance-setup inst))))))))
          (when (turtles--term-truecolor-p (turtles-instance-terminal inst))
            ;; COLORTERM=truecolor tells Emacs to use 24bit terminal
            ;; colors even if the termcap entry doesn't define that.
            ;; That works as long as the Emacs-side terminal supports 24bit colors,
            ;; which is the case for eat and term.el in Emacs 29.1 and later.
            (setq cmdline `("env" "COLORTERM=truecolor" . ,cmdline)))
          (turtles--term-exec
           (turtles-instance-terminal inst)
           cmdline
           (turtles-instance-width inst)
           (turtles-instance-height inst)))

        (setq-local face-remapping-alist turtles-term-face-remapping-alist)
        (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
        (turtles-io-wait-until
         (lambda () (turtles-instance-conn inst))
         (lambda () "Turtles Emacs failed to connect")
         :max-wait-time 0.25)
        (message "Turtles started %s: %s" (turtles-instance-id inst)
                 (or (turtles-instance-shortdoc inst) ""))))

    (with-current-buffer (turtles-instance-term-buf inst)
      (when (turtles--term-resize (turtles-instance-terminal inst)
                                  (turtles-instance-width inst)
                                  (turtles-instance-height inst))
        (turtles--let-term-settle inst)))

    inst))

(defun turtles-stop-instance (inst-or-id)
  "Start instance INST-OR-ID.

INST-OR-ID can be a `turtles-instance' or instance id.

Returns the `turtles-instance'.

Does nothing if the instance is already running."
  (interactive
   (list
    (turtles-read-instance
     "Instance to stop: " #'turtles-instance-live-p)))
  (let ((inst (turtles-get-instance inst-or-id)))
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
        (kill-buffer b)))

    inst))

(defun turtles-read-instance (&optional prompt predicate)
  "Ask the user to select an instance.

Displays PROMPT and let the user choose among instances that
match PREDICATE. Returns the `turtles-instance' that was chosen
or nil.

If specified, PREDICATE must be a function that takes a single
argument of type `turtles-instance'."
  (let ((completion-extra-properties
         '(:annotation-function
           (lambda (id-as-str)
             (when-let* ((inst (turtles-get-instance (intern id-as-str)))
                         (doc (turtles-instance-shortdoc inst)))
               (concat " " doc)))))
        (predicate (if predicate
                       (lambda (cell)
                         (funcall predicate (cdr cell)))
                     #'always)))
    (unless (delq nil (mapcar predicate turtles-instance-alist))
      (error "No appropriate instances"))
    (let ((choice (completing-read
                   (or prompt "Instance: ")
                   turtles-instance-alist predicate
                   'require-match nil 'turtles-read-instance-history)))
      (when choice
        (alist-get (intern choice) turtles-instance-alist)))))

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
  (advice-add 'message :after #'turtles--send-message-upstream)
  (add-hook 'turtles-io-unreadable-obj-functions
            (lambda (obj)
              (setcdr obj (plist-put (cdr obj) :instance instance-id))))
  (setq turtles--upstream
        (turtles-io-connect
         socket
         `((eval
            . ,(turtles-io-method-handler (expr) (eval expr)))
           (ert-test
            . ,(let ((initial-frame (selected-frame)))
                 (lambda (conn id _method expr)
                   (catch 'turtles-return
                     (condition-case err
                         (with-selected-frame initial-frame
                           (when setup-func
                             (funcall setup-func)))
                       ((error t)
                        (turtles-io--send conn `(:id ,id :error ,err))
                        (throw 'turtles-return nil)))
                     (condition-case-unless-debug err
                         (turtles-io--send
                          conn `(:id ,id :result ,(with-selected-frame initial-frame
                                                    (eval expr))))
                       ((error t) (turtles-io--send conn `(:id ,id :error ,err))))))))
           (exit
            . ,(lambda (_conn _id _method _params)
                 (kill-emacs nil))))))
  (turtles-io-notify turtles--upstream 'register instance-id))

(defun turtles--send-message-upstream (msg &rest args)
  "Send a message to the server."
  (when (and turtles-send-messages-upstream
             (turtles-upstream)
             (not (> turtles--sending-messages-up 0)))
    (turtles--with-incremented-var turtles--sending-messages-up
      (turtles-io-notify (turtles-upstream) 'message
                         (concat (format "[%s] " (turtles-this-instance))
                                 (apply #'format msg args))))))

(defun turtles-new-frame-in-instance (inst-or-id)
  "Ask the instance INST-OR-ID to create a new frame.

This opens a new frame on the Emacs instance run by turtles on a
window system, which is convenient for debugging.

The frame that is created is on the same display as the current
frame, which only makes sense for graphical displays."
  (interactive
   (list
    (turtles-read-instance
     "Instance: " #'turtles-instance-live-p)))
  (let ((inst (turtles-get-instance inst-or-id)))
    (unless (turtles-instance-live-p inst)
      (error "Instance not running: %s" (turtles-instance-id inst)))
    (let ((params (frame-parameters)))
      (unless (alist-get 'window-system params)
        (error "No window system"))
      (message "New client frame for %s: %s"
               (turtles-instance-id inst)
               (turtles-instance-eval inst
                 `(progn
                    (prin1-to-string
                     (make-frame
                      '((window-system . ,(alist-get 'window-system params))
                        (display . ,(alist-get 'display params)))))))))))

(defun turtles--let-term-settle (inst)
  "Wait until the Emacs process of INST is done updating the buffer."
  (when-let ((p (turtles-instance-term-proc inst)))
    (when (accept-process-output p 0.05)
      (accept-process-output p 0))))

(provide 'turtles-instance)

;;; turtles-instance.el ends here
