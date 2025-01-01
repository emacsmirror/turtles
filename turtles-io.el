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
(require 'subr-x) ;; when-let

(defvar print-unreadable-function nil) ;; Emacs 29.1

(cl-defstruct (turtles-io-server
               (:constructor turtles-io--make-server)
               (:copier nil))
  "A server created by `turtles-io-server'."

  (proc nil :documentation "The network process used by the server")
  (connections nil :documentation "List of connected clients")
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

(define-error 'turtles-io-unknown-method "Unknown method")
(define-error 'turtles-io-timeout "Operation timed out")

(defvar turtles-io-unreadable-obj-props nil
  "Properties added to unreadable objects.

This is useful to add properties that identify the current
process to the objects it retruns.")

(defvar-local turtles-io--marker nil
  "Marker used in `turtles-io--connection-filter' for reading object.")

(defvar turtles-io--servers nil
  "Set of live servers.

This is used solely to remove the socket file of any remaining
server when Emacs is killed.")

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

(defun turtles-io-server (socket &optional method-alist)
  "Create a new server.

SOCKET is the path at which the server must create a Unix socket.
This can be accessed later using `turtles-io-server-socket'.

METHOD-ALIST is an alist method handlers to pass to client
connections. See `turtles-io-conn-method-alist' for details.

Return an instance of type `turtles-io-server'."
  (add-hook 'kill-emacs-hook #'turtles-io--delete-socket-files-on-exit)
  (when (file-exists-p socket)
    (delete-file socket))
  (let* ((server (turtles-io--make-server
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
    (push server turtles-io--servers)
    (continue-process proc)

    server))

(defun turtles-io--delete-socket-files-on-exit ()
  "Delete the socket files at shutdown.

By default, Emacs will kill the processes, but will leave the
socket files behind."
  (dolist (server turtles-io--servers)
    (let ((socketfile (turtles-io-server-socket server)))
      (when (file-exists-p socketfile)
        (delete-file socketfile)))))

(defun turtles-io-connect (socket &optional method-alist)
  "Connect to a remote server listening at SOCKET.

METHOD-ALIST is an alist of method handlers. See
`turtles-io-conn-method-alist' for details.

Return a `turtles-io-conn' instance."
  (let* ((buf (generate-new-buffer " *turtles-io-client*"))
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

(cl-defmacro turtles-io-handle-method ((conn id) &rest body)
  "Execute BODY to handle the current method.

This macro evaluates BODY, then send the result to CONN. If BODY
raises an error, send an error response to CONN.

ID identifies the source of the response to the remote end. If it
is empty, no result is sent."
  (declare (indent 1))
  (let ((id-var (make-symbol "id"))
        (result-var (make-symbol "result")))
    `(let* ((,id-var ,id)
            (,result-var (condition-case err
                             `(:result ,(progn ,@body))
                           ;; Just t would be enough on Emacs 29, but
                           ;; Emacs 26 doesn't support catching
                           ;; everything with t.
                           ((error t) `(:error ,err)))))
       (when ,id-var
         (turtles-io--send
          ,conn
          `(:id ,,id-var . ,,result-var))))))

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

(defun turtles-io-call-method-async (conn method params handler)
  "Call METHOD on CONN with parameters PARAMS.

This function calls a method and expects the response to be
passed back to HANDLER. HANDLER should be a function that takes
two arguments: a result and an error, only on of which is ever
set at a time.

Returns immediately after the request is sent. If you'd like to
wait for the response, use `turtles-io-call-method '
instead."
  (let ((id (cl-incf (turtles-io-conn-last-id conn))))
    (setf (alist-get id (turtles-io-conn-response-alist conn)) handler)
    (turtles-io--send conn `(:id ,id :method ,method :params ,params))))

(defun turtles-io-notify (conn method &optional params)
  "Call METHOD on CONN with parameters PARAMS with no id.

This function behaves like `turtles-io-call-method-async', except it
doesn't expect a response, so doesn't even bother setting the id."
  (turtles-io--send conn `(:method ,method :params ,params)))

(cl-defun turtles-io-call-method  (conn method &optional params &key timeout)
  "Call METHOD on CONN with PARAMS and wait for the result.

Only wait up to TIMEOUT seconds for the result."
  (let (got-response received-result received-error)
    (turtles-io-call-method-async
     conn method params
     (lambda (result err)
       (setq received-error err)
       (setq received-result result)
       (setq got-response t)))
    (turtles-io-wait-until
     (lambda () got-response)
     (lambda () (format "Timed out waiting for answer for method %s" method))
     :proc (turtles-io-conn-proc conn)
     :timeout (or timeout 5.0))
    (cond ((and (consp received-error)
                (car received-error)
                (symbolp (car received-error)))
           ;; This might signal symbols that are defined as errors on the
           ;; client side, but not on the server side, so won't be caught
           ;; by (condition-case ... (error ...)).
           (signal (car received-error) (cdr received-error)))
          (received-error
           (error "Remote method %s failed: %s"
                  method received-error)))

    received-result))

(defun turtles-io--send (conn msg)
  "Send MSG to CONN.

MSG can be any Lisp object that can be printed."
  (with-temp-buffer
    (turtles-io--print-msg msg)
    ;;(message "send[%s/%s]: %s" (turtles-io-conn-proc conn) (emacs-pid) (buffer-substring-no-properties (point-min) (point-max)))
    (insert "\n\"\"\"\n")
    ;; """ will not appear in a stream generated by prin1.

    (process-send-string (turtles-io-conn-proc conn) (buffer-string))))

(defun turtles-io--print-msg (msg)
  "Print the MSG to the current buffer.

Any unreadable object inside MSG are transformed into
placeholders, so the result can always safely be read back."
  (let ((start-pos (point))
        ;; Hardcode most print settings, so we get consistent
        ;; behavior.
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
        (print-unreadable-function
         (when (>= emacs-major-version 29)
           (turtles-io--make-print-unreadable-func))))
    (prin1 msg (current-buffer))

    (unless print-unreadable-function
      (turtles-io--rewrite-unreadables start-pos (point)))))

(defun turtles-io--make-print-unreadable-func ()
  "Return a function that transforms unreadable OBJ.

The returned function transform OBJ into a readable placeholder.

This is meant to be used as `print-unreadable-function'."
  (let ((closing
           ;; It would be much more convenient to call prin1-to-string
           ;; on each objects, unfortunately calling prin1 from this
           ;; print-unreadable-function messes up the output, at least
           ;; in Emacs 29.
           (if turtles-io-unreadable-obj-props
               (concat
                " "
                (substring (prin1-to-string turtles-io-unreadable-obj-props) 1 -1)
                ")")
             ")")))
    (lambda (obj _escaped)
      (concat
       "(turtles-"
       (pcase (type-of obj)
         ('buffer
          (if (buffer-live-p obj)
              (format "buffer :name %s"
                      (turtles-io--quote-str (buffer-name obj)))
            "buffer :live nil"))
         ('process
          (format "process :name %s"
                  (turtles-io--quote-str (process-name obj))))
         ('frame
          (if (frame-live-p obj)
              (format "frame :name %s"
                      (turtles-io--quote-str
                       (alist-get 'name (frame-parameters obj))))
            "frame :live nil"))
         ('window
          (if-let ((buf (window-buffer obj)))
              (format "window :buffer %s"
                      (turtles-io--quote-str (buffer-name buf)))
            "window"))
         ('marker
          (if-let ((pos (marker-position obj))
                   (buf (marker-buffer obj)))
              (format "marker :pos %d :buffer %s"
                      (marker-position obj)
                      (turtles-io--quote-str (buffer-name buf)))
            "marker"))
         ('overlay
          (format "overlay :from %d :to %d :buffer %s"
                  (overlay-start obj)
                  (overlay-end obj)
                  (turtles-io--quote-str
                   (buffer-name (overlay-buffer obj)))))
         (type (format "obj :type %s" type)))
       closing))))

(defun turtles-io--quote-str (str)
  "Quote STR for Emacs.

This is a poor replacement for `prin1-to-string', to be used in
`print-unreadable-function', during which `prin1' cannot safely
be called."
  (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" str) "\""))

(defun turtles-io--rewrite-unreadables (start-pos end-pos)
  "Transform unreadable objects into something readable.

This function processes the buffer region between START-POS and
END-POS that should be the output of `prin1', transforming
unreadables into a cons (turtles-typename) containing optionally
a plist.

This should work for the most common types found it tests, such
as buffers, markers and overlay. However, this is hackish,
incomplete and might still fail if it encounters
unfortunately-named dead frames or unsupported objects.

This is only useful before Emacs 29.1, as setting
`print-unreadable-function' is much safer and simpler."
  (let ((end-pos (copy-marker end-pos)))
    (unwind-protect
        (save-excursion
          (goto-char start-pos)
          (while (turtles-io--search-unreadable end-pos)
            ;; point is after "#<"
            (let* ((obj-start (point)) obj)
              (setq
               obj
               (cond
                ;; #<killed-buffer>
                ((looking-at "killed buffer>")
                 (goto-char (match-end 0))
                 '(turtles-buffer :live nil))

                ;; #<buffer <buffer-name>>
                ((looking-at "buffer ")
                 (turtles-io--match-unreadable
                  (buffer-list)
                  (lambda (b)
                    (format "\\(%s\\)" (regexp-quote (buffer-name b)))))
                 `(turtles-buffer :name ,(match-string 1)))

                ;; #<process <process-name>>
                ((looking-at "process ")
                 (turtles-io--match-unreadable
                  (process-list)
                  (lambda (p)
                    (format "\\(%s\\)" (regexp-quote (process-name p)))))
                 `(turtles-process :name ,(match-string 1)))

                ;; #<frame <frame-name> 0x[0-9a-f]+>
                ((looking-at "frame ")
                 (turtles-io--match-unreadable
                  (frame-list)
                  (lambda (f)
                    (format "\\(%s\\) 0x[0-9a-f]+"
                            (regexp-quote (alist-get 'name (frame-parameters f))))))
                 `(turtles-frame :name ,(match-string 1)))

                ;; #<dead frame <frame-name> 0x[0-9a-f]+>
                ((looking-at "dead frame ")
                 ;; Dead frames still include the name. We can but
                 ;; hope that it doesn't contain any >.
                 (search-forward ">")
                 `(turtles-frame :live nil))

                ;; #<window [0-9]+>
                ((looking-at "window [0-9]+>")
                 (goto-char (match-end 0))
                 `(turtles-window))

                ;; #<window [0-9]+ on <buffer-name>>
                ((looking-at "window ")
                 (turtles-io--match-unreadable
                  (buffer-list)
                  (lambda (b)
                    (format "[0-9]+ on \\(%s\\)" (regexp-quote (buffer-name b)))))
                 `(turtles-window :buffer ,(match-string 1)))

                ;; #<marker in no buffer>
                ((looking-at "marker \\((moves after insertion) \\)?in no buffer>")
                 (goto-char (match-end 0))
                 `(turtles-marker))

                ;; #<marker at [0-9]+ in <buffer-name>>
                ((looking-at "marker \\((moves after insertion) \\)?")
                 (turtles-io--match-unreadable
                  (buffer-list)
                  (lambda (b) (format "at \\([0-9]+\\) in \\(%s\\)"
                                      (regexp-quote (buffer-name b)))))
                 `(turtles-marker
                   :pos ,(string-to-number (match-string 1))
                   :buffer ,(match-string 2)))

                ;; #<overlay from [0-9]+ to [0-9]+ on <buffer-name>>
                ((looking-at "overlay ")
                 (turtles-io--match-unreadable
                  (buffer-list)
                  (lambda (b) (format "from \\([0-9]+\\) to \\([0-9]+\\) in \\(%s\\)"
                                      (regexp-quote (buffer-name b)))))
                 `(turtles-overlay
                   :from ,(string-to-number (match-string 1))
                   :to ,(string-to-number (match-string 2))
                   :buffer ,(match-string 3)))

                ((looking-at "\\(killed \\|dead \\)?\\([A-Za-z_-]+\\)\\( .*?\\)?>")
                 (goto-char (match-end 0))
                 `(turtles-obj :type ,(intern (match-string 2))))
                (t (error "Cannot parse unreadable %s"
                          (buffer-substring-no-properties
                           (- obj-start 2) (min end-pos (+ obj-start 30)))))))
              ;; point is after the closing ">"
              (delete-region (- obj-start 2) (point))
              (when turtles-io-unreadable-obj-props
                (setcdr obj (append (cdr obj) turtles-io-unreadable-obj-props)))
              (prin1 obj (current-buffer)))))

      (move-marker end-pos nil))))

(defun turtles-io--match-unreadable (list regexp-func)
  "Find a matching unreadable regexp.

This function builds a list of regexps by applying REGEXP-FUNC on
LIST and tries to apply one of them in order just after the end
of the current match.

It fails if it cannot find a match.

Check the match data after a successful return for information
about what matched."
  (let ((start-pos (match-beginning 0))
        (names (sort (mapcar regexp-func list)
                     (lambda (a b) (> (length a) (length b))))))
    (goto-char (match-end 0))
    (catch 'turtles-return
      (dolist (name-regexp names)
        (when (looking-at (concat name-regexp ">"))
          (goto-char (match-end 0))
          (throw 'turtles-return nil)))
      (error "Failed to parse unreadable: #<%s…"
             (buffer-substring-no-properties start-pos (min (point-max) (+ 30 (point))))))))

(defun turtles-io--search-unreadable (limit)
  "Search for the start of an unreadable object from point to LIMIT.

This function goes through the region from point to LIMIT looking
for a #<, which signals the opening of an unreadable object. It
ignores escaped and quoted #<.

Returns non-nil when an opening #< was found, with the point just
after the #<. Return nil once LIMIT was reached without finding
any unreadable object."
  (let ((escaped nil) (in-quote nil) (dash nil))
    (while (and (< (point) limit) (not (and dash (eq (char-after) ?<))))
      (when dash (setq dash nil))
      (let ((c (char-after)))
        (cond
         (escaped
          (setq escaped nil))
         ((eq c ?\\)
          (setq escaped t))
         ((eq c ?\")
          (setq in-quote (not in-quote)))
         ((and (not in-quote) (eq c ?#))
          (setq dash t))))
      (goto-char (1+ (point)))))
  (when (eq (char-after) ?<)
    (goto-char (1+ (point)))

    t))

(defun turtles-io--server-sentinel (server proc)
  "Process sentinel for server connections.

SERVER is the server this sentinel belongs to, a
turtles-io-server instance.

PROC is either the process of the new connection or the server
process, depending on which connection status is being reported.."
  ;; New connection
  (when (and (eq (process-status proc) 'open)
             (not (process-contact proc :server)))
    ;;(message "[%s] new connection %s" (turtles-io-server-proc server) proc)
    (set-process-query-on-exit-flag proc nil)
    (let ((conn (turtles-io--make-conn
                 :proc proc
                 :method-alist (turtles-io-server-method-alist server))))
      (set-process-filter proc
                          (lambda (_proc string)
                            (turtles-io--connection-filter conn string)))
      (push conn (turtles-io-server-connections server))
      (process-put proc 'turtles-io-conn conn)))

  (when (eq (process-status proc) 'closed)
    (if (process-contact proc :server)
        ;; Server stopped
        (progn
          (ignore-errors
            (delete-file (turtles-io-server-socket server)))
          (setq turtles-io--servers (delete server turtles-io--servers)))
      ;; Client connection closed
      (setf (turtles-io-server-connections server)
            (delq (process-get proc 'turtles-io-conn)
                  (turtles-io-server-connections server))))))

(defun turtles-io--connection-filter (conn string)
  "Process STRING sent to CONN."
  (with-current-buffer (process-buffer (turtles-io-conn-proc conn))
    (insert string)

    (while (save-excursion
             (goto-char (point-min))
             (search-forward "\"\"\"\n" nil 'noerror))
      (unless (and (boundp 'turtles-io--marker) turtles-io--marker)
        (setq-local turtles-io--marker (copy-marker (point-min))))
      (let ((end (match-end 0)) obj)
        ;;(message "read[%s/%s]: %s" (turtles-io-conn-proc conn) (emacs-pid) (buffer-substring-no-properties (point-min) end))
        (setq obj (unwind-protect
                      (read turtles-io--marker)
                    ;; Consume the region up to """ whether processing it
                    ;; succeeded or not.
                    (delete-region (point-min) end)))
        ;; Use timer as a run queue of sorts, so commands that fail or
        ;; commands like (top-level), which break the flow, don't
        ;; destroy message processing.
        (run-at-time 0 nil #'turtles-io--dispatch conn obj)))))

(defun turtles-io--dispatch (conn msg)
  "Dispatch a MSG received on CONN to the method or response alists."
  ;;(message "dispatch[%s/%s]: %s" (turtles-io-conn-proc conn) (emacs-pid) msg)
  (let ((id (plist-get msg :id))
        (method (plist-get msg :method))
        (has-result (plist-member msg :result))
        (err (plist-get msg :error)))
    (cond
     ;; method call
     (method
      (with-temp-buffer ;; Isolates handlers.
        (funcall (or (alist-get method (turtles-io-conn-method-alist conn))
                     #'turtles-io--default-method-handler)
                 conn id method (plist-get msg :params))))

     ;; call response
     ((or has-result err)
      (if-let ((handler (alist-get id (turtles-io-conn-response-alist conn))))
          (with-temp-buffer (funcall handler (plist-get msg :result) err))
        (warn "Unexpected response: %s" msg)))

     ;; invalid
     (t (warn "Malformed message: %s" msg)))))

(defun turtles-io--default-method-handler (conn id _method _params)
  "Handle an unsupported method with ID received from CONN."
  (turtles-io-send-error conn id '(turtles-io-unknown-method)))

(cl-defun turtles-io-wait-until (predicate on-timeout &key max-wait-time proc (timeout 5))
  "Wait for up to TIMEOUT seconds for PREDICATE to become non-nil.

Fails with a signal \\='turtles-io-timeout and the error message
returned by ON-TIMEOUT if it times out.

This function assumes that PREDICATE becomes non-nil as a result
of processing some process output. If that's not always the case,
set MAX-WAIT-TIME to some small, but reasonable value.

If PROC is non nil, tell Emacs to only accept output from that
process while waiting. This can dramatically speed up operations
when waiting results from a specific process.

On timeout, sends a signal of type `turtles-io-timeout'"
  (let ((start (current-time)) remaining)
    (while (not (funcall predicate))
      (setq remaining (- timeout
                         (time-to-seconds
                          (time-subtract (current-time) start))))
      (unless (> remaining 0)
        (signal 'turtles-io-timeout (funcall on-timeout)))
      (when (and max-wait-time (> remaining max-wait-time))
        (setq remaining max-wait-time))
      (accept-process-output proc remaining))))

(provide 'turtles-io)

;;; turtles-io.el ends here
