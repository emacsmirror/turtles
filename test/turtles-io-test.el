;; turtles-io-test.el --- Test turtles-io.el -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025 Stephane Zermatten

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

(require 'ert)
(require 'ert-x)
(require 'compat)

(require 'turtles-io)

(ert-deftest turtles-io-send-message-to-server ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server (turtles-io-server
                          socket
                          `((ping . ,(lambda (conn id method params)
                                       (turtles-io-send-result conn id "pong"))))))
            (should (turtles-io-server-p server))
            (should (process-live-p (turtles-io-server-proc server)))
            (should (equal socket (turtles-io-server-socket server)))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should (equal "pong" (turtles-io-call-method  client 'ping))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-delete-socket-file ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir)) server)
      (unwind-protect
          (progn
            (setq server (turtles-io-server socket))
            (should (file-exists-p socket))
            (delete-process (turtles-io-server-proc server))
            (turtles-io-wait-until
             (lambda () (not (file-exists-p socket)))
             (lambda () "Server did not delete socket")))

        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-send-message-to-client ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server (turtles-io-server socket))
            (should (turtles-io-server-p server))
            (setq client (turtles-io-connect
                          socket `((ping . ,(lambda (conn id method params)
                                              (turtles-io-send-result conn id "pong"))))))
            (turtles-io-wait-until
             (lambda () (turtles-io-server-connections server))
             (lambda () "Client not connected")
             :max-wait-time 0.1)

            (should (equal "pong" (turtles-io-call-method
                                   (car (turtles-io-server-connections server)) 'ping))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))


(ert-deftest turtles-io-call-unknown-method ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server (turtles-io-server socket))
            (setq client (turtles-io-connect socket))

            (condition-case err
                (turtles-io-call-method  client 'ping)
              (turtles-io-unknown-method
               (should (equal '(turtles-io-unknown-method) err)))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-handle-method ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((inc . ,(lambda (conn id _method index)
                               (turtles-io-handle-method (conn id)
                                 (1+ index)))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should (equal 2 (turtles-io-call-method  client 'inc 1))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-handle-method-with-error ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((inc . ,(lambda (conn id _method index)
                               (turtles-io-handle-method (conn id)
                                 (1+ index)))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should-error (turtles-io-call-method  client 'inc "cannot-add")
                          :type 'wrong-type-argument)
            )

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-handle-method-with-error-bad-symbol ()
  ;; Emacs 26 doesn't allow catching non-error symbols
  ;; using t as a condition, so this test just isn't possible.
  ;;
  ;; Using t as a condition in condition-case might have been added
  ;; before Emacs 29. This is likely too aggressive a skip.
  (skip-unless (>= emacs-major-version 29))

  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((inc . ,(lambda (conn id _method index)
                               (turtles-io-handle-method (conn id)
                                 (signal 'fake-error "foobar")))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should (equal '(fake-error . "foobar")
                           (condition-case err
                               (turtles-io-call-method
                                client 'inc "cannot-add")
                             (t err)))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-handle-method-with-error-not-a-symbol ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((inc . ,(lambda (conn id method _params)
                               (turtles-io-send-error conn id '("error string" 3)))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should
             (equal '(error "Remote method inc failed: (error string 3)")
                    (condition-case err
                        (prog1 nil
                          (turtles-io-call-method  client 'inc "cannot-add"))
                      (error err)))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-method-return-nil ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((ping . ,(lambda (conn id _method _params)
                                (turtles-io-handle-method (conn id)
                                  nil))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should (null (turtles-io-call-method  client 'ping))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-method-return-unreadable ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((get-unreadable . ,(lambda (conn id _method _params)
                                          (turtles-io-handle-method (conn id)
                                            (current-buffer)))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (let ((expr (turtles-io-call-method  client 'get-unreadable)))
              (should (consp expr))
              (should (equal 'turtles-buffer (nth 0 expr)))
              (should (equal :name (nth 1 expr)))
              (should (stringp (nth 2 expr)))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-notify ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          (ping-count 0)
          server client)
      (unwind-protect
          (progn
            (setq server (turtles-io-server
                          socket
                          `((ping . ,(lambda (conn id _method _params)
                                       (turtles-io-handle-method (conn id)
                                         (cl-incf ping-count)))))))
            (setq client (turtles-io-connect socket))

            (turtles-io-notify client 'ping)
            (turtles-io-notify client 'ping)
            (turtles-io-wait-until
             (lambda () (equal 2 ping-count))
             (lambda () "Not enough pings")))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(defun turtles-io--print-msg-to-string (msg)
  (with-temp-buffer
    (turtles-io--print-msg msg)
    (buffer-string)))

(ert-deftest turtles-io-print-msg ()
  (should (equal "(:id 12 :method ping :params \"ping\")"
                 (turtles-io--print-msg-to-string
                  '(:id 12 :method ping :params "ping"))))

  (should (equal "(:id 12 :result (1 2 3))"
                 (turtles-io--print-msg-to-string
                  '(:id 12 :result (1 2 3)))))

  (should (equal "(:id 12 :result nil)"
                 (turtles-io--print-msg-to-string
                  '(:id 12 :result nil))))

  (should (equal "(:id 12 :error (unknown \"unknown\"))"
                 (turtles-io--print-msg-to-string
                  '(:id 12 :error (unknown "unknown")))))

  (should (equal "(:id 12 :result (readable-with-\\#and-\\#< \"#<\" 35 60))"
                 (turtles-io--print-msg-to-string
                  '(:id 12 :result (readable-with-\#\and-\#< "#<" ?# ?<))))))

(ert-deftest turtles-io-print-msg-unreadable ()
  (let ((turtles-io-unreadable-obj-props nil))
    (with-temp-buffer
      (rename-buffer (generate-new-buffer-name "temp >>--#<\""))
      (should (equal (format "(:id 12 :result (1 2 (turtles-buffer :name %s) 3))"
                             (prin1-to-string (buffer-name)))
                     (turtles-io--print-msg-to-string
                      `(:id 12 :result (1 2 ,(current-buffer) 3)))))

      (should (equal "(:id 12 :result (1 2 (turtles-buffer :live nil) 3))"
                     (turtles-io--print-msg-to-string
                      `(:id 12 :result (1 2 ,(with-temp-buffer (current-buffer)) 3)))))

      (with-selected-window (display-buffer (current-buffer))
        (should (equal (format "(:id 12 :result (1 2 (turtles-window :buffer %s) 3))"
                               (prin1-to-string (buffer-name)))
                       (turtles-io--print-msg-to-string
                        `(:id 12 :result (1 2 ,(selected-window) 3))))))

      (insert "some text")
      (should (equal (format "(:id 12 :result (1 2 (turtles-overlay :from 1 :to 5 :buffer %s) 3))"
                             (prin1-to-string (buffer-name)))
                     (turtles-io--print-msg-to-string
                      `(:id 12 :result (1 2 ,(make-overlay 1 5) 3)))))

      (should (equal (format "(:id 12 :result (1 2 (turtles-marker :pos 3 :buffer %s) 3))"
                             (prin1-to-string (buffer-name)))
                     (turtles-io--print-msg-to-string
                      `(:id 12 :result (1 2 ,(copy-marker 3) 3)))))
      (should (equal (format "(:id 12 :result (1 2 (turtles-marker :pos 3 :buffer %s) 3))"
                             (prin1-to-string (buffer-name)))
                     (turtles-io--print-msg-to-string
                      `(:id 12 :result (1 2 ,(copy-marker 3 t) 3))))))

    (should (equal (format "(:id 12 :result (1 2 (turtles-marker) 3))"
                           (prin1-to-string (buffer-name)))
                   (turtles-io--print-msg-to-string
                    `(:id 12 :result (1 2 ,(make-marker) 3)))))

    (should (equal (format "(:id 12 :result (1 2 (turtles-frame :name %s) 3))"
                           (prin1-to-string (alist-get 'name (frame-parameters))))
                   (turtles-io--print-msg-to-string
                    `(:id 12 :result (1 2 ,(selected-frame) 3)))))

    (should (equal "(:id 12 :result (1 2 (turtles-obj :type window-configuration) 3))"
                   (turtles-io--print-msg-to-string
                    `(:id 12 :result (1 2 ,(current-window-configuration) 3)))))))

(ert-deftest turtles-io-print-msg-unreadable-hook ()
  (let ((turtles-io-unreadable-obj-props '(:foo bar)))
    (with-temp-buffer
      (should (equal (format "(turtles-buffer :name %s :foo bar)"
                             (prin1-to-string (buffer-name)))
                     (turtles-io--print-msg-to-string (current-buffer)))))))

(defun turtles-io--parse-unreadable (str)
  (with-temp-buffer
    (insert str)
    (cl-letf (((symbol-function 'buffer-list)
               (lambda (&rest _) '(mybuffer1 mybuffer2 mybuffer3)))
              ((symbol-function 'buffer-name)
               (lambda (buf &rest _)
                 (cond
                  ((eq buf 'mybuffer1) "my buffer<1>")
                  ((eq buf 'mybuffer2) "my buffer<2>")
                  ((eq buf 'mybuffer3) "my buffer<3>")
                  (t (error "unknown buffer %s" buf))))))
      (turtles-io--rewrite-unreadables (point-min) (point-max)))
    (goto-char (point-min))
    (read (current-buffer))))

(ert-deftest turtles-io-rewrite-unreadable ()
  (should
   (equal
    '(turtles-obj :type INVALID_LISP_OBJECT)
    (turtles-io--parse-unreadable
     "#<INVALID_LISP_OBJECT 0xaf0001>")))
  (should
   (equal
    '(turtles-obj :type SOME_LISP_OBJECT)
    (turtles-io--parse-unreadable
     "#<SOME_LISP_OBJECT 0xaf0001>")))
  (should
   (equal
    '(turtles-marker)
    (turtles-io--parse-unreadable
     "#<marker in no buffer>")))
  (should
   (equal
    '(turtles-marker)
    (turtles-io--parse-unreadable
     "#<marker (moves after insertion) in no buffer>")))
  (should
   (equal
    '(turtles-marker :pos 100 :buffer "my buffer<3>")
    (turtles-io--parse-unreadable
     "#<marker at 100 in my buffer<3>>")))
  (should
   (equal
    '(turtles-marker :pos 100 :buffer "my buffer<3>")
    (turtles-io--parse-unreadable
     "#<marker (moves after insertion) at 100 in my buffer<3>>")))
  (should
   (equal
    '(turtles-buffer :live nil)
    (turtles-io--parse-unreadable "#<killed buffer>")))
  (should
   (equal
    '(turtles-frame :live nil)
    (turtles-io--parse-unreadable "#<dead frame myframe 0xaf01ee03>")))
  (should
   (equal
    '(turtles-obj :type xwidget)
    (turtles-io--parse-unreadable "#<xwidget 0xa701ee03>")))
  (should
   (equal
    '(turtles-obj :type xwidget)
    (turtles-io--parse-unreadable "#<killed xwidget>"))))

(ert-deftest turtles-io-timeout-to-end-time ()
  (cl-letf (((symbol-function 'current-time)
             (lambda ()
               ;; 2025-03-14T21:34:22+0000
               '(26580 41182 758839 0))))
    (should (equal
             "2025-03-14T21:34:52+0000"
             (format-time-string
              "%FT%T%z" (turtles-io--timeout-to-end-time 30.0) t)))
    (should (equal
             '(26500 41182 758839 0)
             (turtles-io--timeout-to-end-time
              '(absolute . (26500 41182 758839 0)))))))

(ert-deftest turtles-io--remaining-seconds ()
  (cl-letf* ((test-time '(26580 41182 758839 0))
             ((symbol-function 'current-time)
             (lambda () test-time)))
    (should (equal 30.0 (turtles-io--remaining-seconds
                         (time-add test-time 30.0))))
    (should (equal -30.0 (turtles-io--remaining-seconds
                          (time-subtract test-time 30.0))))))
