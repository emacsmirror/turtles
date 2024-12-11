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

(require 'turtles-io)

(ert-deftest turtles-io-test-send-message ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server client collected-responses)
      (unwind-protect
          (progn
            (setq server (turtles-io-server
                          socket
                          (lambda (proc msg)
                            (turtles-io-send proc `(from-server . ,msg)))))
            (should (turtles-io-server-p server))
            (should (process-live-p (turtles-io-server-proc server)))
            (should (equal socket (turtles-io-server-socket server)))

            (setq client (turtles-io-connect
                          socket
                          (lambda (proc msg)
                            (push msg collected-responses))))

            (should (process-live-p client))

            (turtles-io-send client '(from-client . "hello"))
            (turtles-io-wait-for 5 "Timed out waiting for a response" (lambda () collected-responses))
            (should (equal '((from-server . (from-client . "hello"))) collected-responses)))

        (ignore-errors (when client (delete-process client)))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-client-connect ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          server clients connected)
      (unwind-protect
          (progn
            (setq server (turtles-io-server
                          socket
                          #'ignore
                          (lambda (proc)
                            (push proc connected))))
            (should (null (turtles-io-server-clients server)))

            (push (turtles-io-connect socket #'ignore) clients)
            (turtles-io-wait-for 5 "Client #1 not connected" (lambda () connected))

            (should (equal 1 (length connected)))
            (should (equal connected (turtles-io-server-clients server)))

            (push (turtles-io-connect socket #'ignore) clients)
            (turtles-io-wait-for 5 "Client #2 not connected" (lambda () (length> connected 1)))

            (should (equal 2 (length connected)))
            (should (equal connected (turtles-io-server-clients server)))

            (delete-process (nth 1 clients))
            (turtles-io-wait-for 5 "Client #1 not disconnected"
                                 (lambda ()
                                   (length< (turtles-io-server-clients server) 2)))

            (should (equal (list (nth 0 connected)) (turtles-io-server-clients server))))

        (dolist (client clients)
          (ignore-errors (when client (delete-process client))))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-send-to-all-clients ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir))
          received-messages handler server clients)
      (unwind-protect
          (progn
            (setq server (turtles-io-server socket #'ignore))
            (should (null (turtles-io-server-clients server)))

            (turtles-io-send server "ignored") ;; there are no clients

            (setq handler (lambda (proc msg)
                            (push (cons proc msg) received-messages)))

            (push (turtles-io-connect socket handler) clients)
            (push (turtles-io-connect socket handler) clients)
            (push (turtles-io-connect socket handler) clients)
            (turtles-io-wait-for 5 "Clients not connected"
                                 (lambda ()
                                   (length= (turtles-io-server-clients server) 3)))

            (turtles-io-send server "foo")
            (turtles-io-wait-for 5 "Clients did not receive messages"
                                 (lambda ()
                                   (length= received-messages 3)))

            (should (equal '("foo" "foo" "foo") (mapcar #'cdr received-messages)))
            (dolist (client clients)
              (should (memq client (mapcar #'car received-messages)))))

        (dolist (client clients)
          (ignore-errors (when client (delete-process client))))
        (ignore-errors (when server (delete-process server)))))))

(ert-deftest turtles-io-test-delete-socket-file ()
  (ert-with-temp-directory dir
    (let ((socket (expand-file-name "socket" dir)) server)
      (unwind-protect
          (progn
            (setq server (turtles-io-server socket #'ignore))
            (should (file-exists-p socket))
            (delete-process (turtles-io-server-proc server))
            (turtles-io-wait-for 1 "Server did not delete socket"
                                 (lambda () (not (file-exists-p socket)))))

        (ignore-errors (when server (delete-process server)))))))
