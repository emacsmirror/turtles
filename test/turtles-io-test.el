;; turtles-io-test.el --- Test turtles-io.el -*- lexical-binding: t -*-

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

(require 'ert)
(require 'ert-x)
(require 'compat)

(require 'turtles-io)

(ert-deftest turtles-io-test-send-message-to-server ()
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

(ert-deftest turtles-io-test-client-connect ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server clients connected)
      (unwind-protect
          (progn
            (setq server (turtles-io-server
                          socket nil
                          (lambda (proc)
                            (push proc connected))))
            (should (null (turtles-io-server-connections server)))

            (push (turtles-io-connect socket) clients)
            (turtles-io-wait-for 5 "Client #1 not connected" (lambda () connected) 0.1)

            (should (equal 1 (length connected)))
            (should (equal connected (turtles-io-server-connections server)))

            (push (turtles-io-connect socket) clients)
            (turtles-io-wait-for 5 "Client #2 not connected" (lambda () (length> connected 1)) 0.1)

            (should (equal 2 (length connected)))
            (should (equal connected (turtles-io-server-connections server)))

            (delete-process (turtles-io-conn-proc (nth 1 clients)))
            (turtles-io-wait-for 5 "Client #1 not disconnected"
                                 (lambda ()
                                   (length< (turtles-io-server-connections server) 2)) 0.1)

            (should (equal (list (nth 0 connected)) (turtles-io-server-connections server))))

        (dolist (client clients)
          (ignore-errors (when client (delete-process client))))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-delete-socket-file ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir)) server)
      (unwind-protect
          (progn
            (setq server (turtles-io-server socket))
            (should (file-exists-p socket))
            (delete-process (turtles-io-server-proc server))
            (turtles-io-wait-for 1 "Server did not delete socket"
                                 (lambda () (not (file-exists-p socket)))))

        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-send-message-to-client ()
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
            (turtles-io-wait-for 5 "Client not connected"
                                 (lambda () (turtles-io-server-connections server)) 0.1)

            (should (equal "pong" (turtles-io-call-method
                                   (car (turtles-io-server-connections server)) 'ping))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))


(ert-deftest turtles-io-test-call-unknown-method ()
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

(ert-deftest turtles-io-test-method-handler ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((inc . ,(turtles-io-method-handler (index)
                               (1+ index))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should (equal 2 (turtles-io-call-method  client 'inc 1))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-method-handler-with-error ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((inc . ,(turtles-io-method-handler (index)
                               (1+ index))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should-error (turtles-io-call-method  client 'inc "cannot-add")
                          :type 'wrong-type-argument)
            )

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-method-handler-with-error-bad-symbol ()
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
                   `((inc . ,(turtles-io-method-handler (index)
                               (signal 'fake-error "foobar"))))))

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

(ert-deftest turtles-io-test-method-handler-with-error-not-a-symbol ()
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

(ert-deftest turtles-io-test-method-return-nil ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((ping . ,(turtles-io-method-handler (index)
                               nil)))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (should (null (turtles-io-call-method  client 'ping))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-method-return-unreadable ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server
                  (turtles-io-server
                   socket
                   `((get-unreadable . ,(turtles-io-method-handler (index)
                                          (list "before" (current-buffer) "after"))))))

            (setq client (turtles-io-connect socket))
            (should (turtles-io-conn-p client))
            (should (process-live-p (turtles-io-conn-proc client)))

            (let ((expr (turtles-io-call-method  client 'get-unreadable)))
              (should (equal "before" (nth 0 expr)))
              (should (equal "after" (nth 2 expr)))
              (should (equal 'unreadable (car (nth 1 expr))))
              (should (string-match-p "buffer  \\*temp.*" (nth 1 (nth 1 expr))))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-notify ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          (ping-count 0)
          server client)
      (unwind-protect
          (progn
            (setq server (turtles-io-server
                          socket
                          `((ping . ,(turtles-io-method-handler (_ignored)
                                       (cl-incf ping-count))))))
            (setq client (turtles-io-connect socket))

            (turtles-io-notify client 'ping)
            (turtles-io-notify client 'ping)
            (turtles-io-wait-for 5 "Not enough pings"
                                 (lambda () (equal 2 ping-count))))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))
