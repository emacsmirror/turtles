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
                                          (current-buffer))))))

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

(defun turtles-io--print-msg-to-string (msg)
  (with-temp-buffer
    (turtles-io--print-msg msg)
    (buffer-string)))

(ert-deftest turtles-io-test-print-msg ()
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

(ert-deftest turtles-io-test-print-msg-unreadable ()
  (with-temp-buffer
    (rename-buffer (generate-new-buffer-name "temp >>--#<\""))
    (should (equal (format "(:id 12 :result (1 2 (turtles-buffer :name %s) 3))"
                           (prin1-to-string (buffer-name)))
                   (turtles-io--print-msg-to-string
                    `(:id 12 :result (1 2 ,(current-buffer) 3)))))

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
                    `(:id 12 :result (1 2 ,(copy-marker 3) 3))))))

  (should (equal (format "(:id 12 :result (1 2 (turtles-frame :name %s) 3))"
                         (prin1-to-string (alist-get 'name (frame-parameters))))
                 (turtles-io--print-msg-to-string
                  `(:id 12 :result (1 2 ,(selected-frame) 3)))))

  (should (equal "(:id 12 :result (1 2 (turtles-obj \"window-configuration\") 3))"
                 (turtles-io--print-msg-to-string
                  `(:id 12 :result (1 2 ,(current-window-configuration) 3))))))
