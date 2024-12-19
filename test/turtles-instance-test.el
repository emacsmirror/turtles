;;; turtles-instance-test.el --- Test turtles-instance.el -*- lexical-binding: t -*-

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

(require 'compat)
(require 'ert)
(require 'ert-x)
(require 'turtles-instance)

(turtles-definstance turtles-test-restart ()
  "A one-off test instance to test restart.")

(ert-deftest turtles-instance-test-restart ()
  (turtles-start-server)
  (should turtles--server)
  (should (turtles-io-server-live-p turtles--server))

  (let ((inst (turtles-get-instance 'turtles-test-restart))
        buf proc)
    (should inst)
    (turtles-stop-instance inst)
    (turtles-start-instance inst)
    (should (turtles-instance-live-p inst))

    (setq buf (turtles-instance-term-buf inst))
    (should (buffer-live-p buf))
    (should (process-live-p (get-buffer-process buf)))

    (setq proc (turtles-io-conn-proc (turtles-instance-conn inst)))
    (should (process-live-p proc))

    (should (equal "ok" (turtles-io-call-method
                         (turtles-instance-conn inst) 'eval "ok")))


    (turtles-stop-instance inst)

    (should-not (turtles-instance-live-p inst))
    (should-not (buffer-live-p buf))
    (should-not (process-live-p proc))))

(ert-deftest turtles-instance-test-message ()
  (let ((inst (turtles-get-instance 'default)))
    (should inst)
    (turtles-start-instance inst)

    (ert-with-message-capture messages
      (let* ((pid (turtles-io-call-method
                   (turtles-instance-conn inst)
                   'eval
                   `(progn
                      (message "hello from turtles-test-message")
                      (emacs-pid))))
             (message (format "[PID %s] hello from turtles-test-message" pid)))
        (unless (member message (string-split messages "\n" 'omit-nulls))
          (error "message not found in %s" messages))))))

(require 'turtles-instance)
