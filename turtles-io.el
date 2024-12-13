;;; turtles-io.el --- Socket communication between Emacs processes -*- lexical-binding: t -*-

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
;; This package defines a socket-based communication mechanism between
;; Emacs instances based on elisp objects. The protocol is freely
;; adapted from JSON-RPC.
;;

;;; Code:
(require 'cl-lib)

(cl-defstruct (turtles-io-server
               (:constructor turtles-io--make-server)
               (:copier nil))
  "A server created by `turtles-io-server'."

  (proc nil :documentation "The network process used by the server")
  (connections nil :documentation "List of connected clients")
  (on-new-connection nil :documentation "Function called when a new client connects")
  (socket nil :read-only t :documentation "Path to the unix socket file used by the server")
  (method-alist nil :read-only t :documentation "Alist of method symbols to method handlers.

Method handlers take three arguments, the request id, the method
symbol and parameters, which might be nil.

This is passed to the connection objects when a new client
connects."))

(cl-defstruct (turtles-io-conn
               (:constructor turtles-io--make-conn)
               (:copier nil))
  "A connection between two Emacs processes."
  (proc nil :documentation "The network process for this connection")
  (alist nil :documentation "Associate arbitrary data to this connection")
  (method-alist nil :read-only t :documentation "Alist of method symbols to method handlers.

Method handlers take four arguments, the connection, the request
id, the method symbol and parameters, which might be nil.")

  (response-alist nil :documentation "Alist of request id to response handlers.

Response handlers take three arguments: result and errors, only
one of which is ever specified.")
  (last-id 0 :documentation "ID of the last method called on this connection"))

(defvar-local turtles-io--marker nil
  "Marker used in `turtles-io--connection-filter' for reading object.")

(defun turtles-io-conn-live-p (conn)
  "Return non-nil if CONN is a connnection with a live process."
  (and conn
       (turtles-io-conn-p conn)
       (turtles-io-conn-proc conn)
       (process-live-p (turtles-io-conn-proc conn))))

(defun turtles-io-server-live-p (server)
  "Return non-nil if SERVER is a server with a live process."
  (and server
       (turtles-io-server-p server)
       (turtles-io-server-proc server)
       (process-live-p (turtles-io-server-proc server))))

(defun turtles-io-server (socket &optional method-alist on-new-connection)
  "Create a new server.

SOCKET is the path at which the server must create a Unix socket.
This can be accessed later using `turtles-io-server-socket'.

METHOD-ALIST is an alist method handlers to pass to client
connections. See `turtles-io-conn-method-alist' for details.

ON-NEW-CONNECTION, if non-nil, is a function that'll be called
whenever a new client connect. It takes a single argument, the
client `turtles-io-conn' instance. This can be accessed and
modified later using `turtles-io-server-on-new-connection'.

Return an instance of type `turtles-io-server'."
  (let* ((server (turtles-io--make-server
                  :on-new-connection on-new-connection
                  :socket socket
                  :method-alist method-alist))
         (proc (make-network-process
                 :name " *turtles-io-server*"
                 :family 'local
                 :service socket
                 :server t
                 :noquery t
                 :stop t
                 :sentinel (lambda (proc _msg)
                             (turtles-io--server-sentinel server proc))
                 :filter nil)))
    (setf (turtles-io-server-proc server) proc)
    (continue-process proc)

    server))

(defun turtles-io-connect (socket &optional method-alist)
  "Connect to a remote server listening at SOCKET.

METHOD-ALIST is an alist of method handlers. See
`turtles-io-conn-method-alist' for details.

Return a `turtles-io-conn' instance."
  (let* ((buf (generate-new-buffer " *turtles-io-client*" t))
         (conn (turtles-io--make-conn :method-alist method-alist))
         (proc (make-network-process
                :name (buffer-name buf)
                :buffer buf
                :family 'local
                :service socket
                :server nil
                :noquery t
                :sentinel #'ignore
                :stop t
                :filter (lambda (_proc string)
                          (turtles-io--connection-filter conn string)))))
    (setf (turtles-io-conn-proc conn) proc)
    (continue-process proc)

    conn))

(cl-defmacro turtles-io-method-handler ((var) &rest body)
  "Build a method handler.

This macro returns a method handler that'll call BODY with VAR
bound to the method parameters and return BODY evaluation result
to the caller.

If BODY signals an error, that error is sent back to the caller
as an error response."
  (declare (indent 1))
  (let ((conn-var (make-symbol "conn"))
        (id-var (make-symbol "id"))
        (method-var (make-symbol "_method")))
  `(lambda (,conn-var ,id-var ,method-var ,var)
     (turtles-io--send
      ,conn-var
      (cons :id (cons ,id-var
                      (condition-case err
                          (cons :result (cons (progn ,@body) nil))
                        (t (cons :error (cons err nil))))))))))

(defun turtles-io-send-error (conn id error)
  "Send an error back to the caller.

CONN is the connection on which the call was made, ID the request
id and ERROR an object describing the error."
  (turtles-io--send conn `(:id ,id :error ,error)))

(defun turtles-io-send-result (conn id result)
  "Send a succesful response back to the caller.

CONN is the connection on which the call was made, ID the request
id and RESULT the result of the call, which might be nil."
  (turtles-io--send conn `(:id ,id :result ,result)))

(defun turtles-io-call-method (conn method params handler)
  "Call METHOD on CONN with parameters PARAMS.

This function calls a method and expects the response to be
passed back to HANDLER. HANDLER should be a function that takes
two arguments: a result and an error, only on of which is ever
set at a time.

Returns immediately after the request is sent. If you'd like to
wait for the response, use `turtles-io-call-method-and-wait'
instead."
  (let ((id (cl-incf (turtles-io-conn-last-id conn))))
    (setf (alist-get id (turtles-io-conn-response-alist conn)) handler)
    (turtles-io--send conn `(:id ,id :method ,method :params ,params))))

(defun turtles-io-call-method-and-wait (conn method &optional params timeout)
  "Call METHOD on CONN with PARAMS and wait for the result.

Only wait up to TIMEOUT seconds for the result."
  (let (got-response received-result received-error)
    (turtles-io-call-method
     conn method params
     (lambda (result err)
       (setq received-error err)
       (setq received-result result)
       (setq got-response t)))
    (turtles-io-wait-for (or timeout 5) "No response from server"
                         (lambda () got-response))

    (pcase received-error
      (`(,(and (pred symbolp) sym) . ,data)
       ;; This might signal symbols that are defined as errors on the
       ;; client side, but not on the server side, so won't be caught
       ;; by (condition-case ... (error ...)).
       (signal sym data))
      ((and (pred (not null)) any)
       (error "Remote method %s failed: %s" method any)))

    received-result))

(defun turtles-io--send (conn msg)
  "Send MSG to CONN.

MSG can be any lisp object that can be printed."
  (with-temp-buffer
    ;; Overriding all print options.
    ;;
    ;; Overriding output variable unreadeable-function (sic) doesn't
    ;; work at least in 29.4 and we need to do that, as otherwise we
    ;; get failure from functions that don't return anything
    ;; interesting but just happen to end with a call that returns
    ;; some unreadable object.
    (let ((print-unreadable-function (lambda (_obj _escape) 'unreadable))
          (print-length nil)
          (print-level nil)
          (print-circle t)
          (print-quoted t)
          (print-escape-newlines nil)
          (print-escape-control-characters t)
          (print-escape-nonascii nil)
          (print-escape-multibyte nil)
          (print-charset-text-property 'default)
          (print-gensym t)
          (print-continuous-numbering nil)
          (print-number-table nil)
          (float-output-format nil)
          (print-integers-as-characters nil))
      (prin1 msg (current-buffer)))
    ;;(message "send: %s" (buffer-substring-no-properties (point-min) (point-max)))
    (insert "\n\"\"\"\n")
    ;; """ will not appear in a stream generated by prin1.

    (process-send-string (turtles-io-conn-proc conn) (buffer-string))))

(defun turtles-io--server-sentinel (server proc)
  "Process sentinel for server connections"
  ;; New connection
  (when (and (eq (process-status proc) 'open)
             (not (process-contact proc :server)))
    (set-process-query-on-exit-flag proc nil)
    (let ((conn (turtles-io--make-conn
                 :proc proc
                 :method-alist (turtles-io-server-method-alist server))))
      (set-process-filter proc
                          (lambda (_proc string)
                            (turtles-io--connection-filter conn string)))
      (push conn (turtles-io-server-connections server))
      (process-put proc 'turtles-io-conn conn)
      (when-let ((f (turtles-io-server-on-new-connection server)))
        (funcall f conn))))

  (when (eq (process-status proc) 'closed)
    (if (process-contact proc :server)
        ;; Server stopped
        (ignore-errors
          (delete-file (turtles-io-server-socket server)))
      ;; Client connection closed
      (setf (turtles-io-server-connections server)
            (delq (process-get proc 'turtles-io-conn)
                  (turtles-io-server-connections server))))))

(defun turtles-io--connection-filter (conn string)
  "Process STRING sent to CONN."
  (with-current-buffer (process-buffer (turtles-io-conn-proc conn))
    (insert string)

    (when (save-excursion
            (search-backward "\"\"\"\n" nil 'noerror))
      (unless (and (boundp 'turtles-io--marker) turtles-io--marker)
        (setq-local turtles-io--marker (copy-marker (point-min))))
      (let ((end (match-end 0)) obj)
        (setq obj (unwind-protect
                      (read turtles-io--marker)
                    ;; Consume the region up to """ whether processing it
                    ;; succeeded or not.
                    (delete-region (point-min) end)))
        (with-temp-buffer
          ;; Something in dispatch sometimes changes the buffer.
          ;; TODO: figure out what's happening
          (turtles-io--dispatch conn obj))))))

(defun turtles-io--dispatch (conn msg)
  "Dispatch a MSG received on CONN to the method or response alists."
  ;;(message "dispatch: %s" msg)
  (let ((id (plist-get msg :id))
        (method (plist-get msg :method))
        (has-result (plist-member msg :result))
        (err (plist-get msg :error)))
    (cond
     ;; method call
     (method
      (funcall (or (alist-get method (turtles-io-conn-method-alist conn))
                   #'turtles-io--default-method-handler)
               conn id method (plist-get msg :params)))

     ;; call response
     ((or has-result err)
      (if-let ((handler (alist-get id (turtles-io-conn-response-alist conn))))
          (funcall handler (plist-get msg :result) err)
        (warn "Unexpected response: %s" msg)))

     ;; invalid
     (t (warn "Malformed message: %s" msg)))))

(define-error 'turtles-io-unknown-method "Unknown method")
(defun turtles-io--default-method-handler (conn id _method _params)
  "Handle an unsupported method with ID received from CONN."
  (turtles-io-send-error conn id '(turtles-io-unknown-method)))

(defun turtles-io-wait-for (timeout error-message predicate &optional max-wait-time)
  "Wait for up to TIMEOUT seconds for PREDICATE to become non-nil.

Fails with ERROR-MESSAGE if it times out.

This function assumes that PREDICATE becomes non-nil as a result
of processing some process output. If that's not always the case,
set MAX-WAIT-TIME to some small, but reasonable value."
  (let ((start (current-time)) remaining)
    (while (and (> (setq remaining
                         (- timeout
                            (time-to-seconds
                             (time-subtract (current-time) start))))
                   0)
                (not (funcall predicate)))
      (when (and max-wait-time (> remaining max-wait-time))
        (setq remaining max-wait-time))
      (accept-process-output nil remaining)))
  (unless (funcall predicate)
    (error (concat error-message))))

(provide 'turtles-io)

;;; turtles-io.el ends here
