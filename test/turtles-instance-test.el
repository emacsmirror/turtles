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

(ert-deftest turtles-start-stop ()
  (unwind-protect
      (progn
        (turtles-start)
        (should turtles--server)
        (should turtles--conn)
        (should (equal "ok" (turtles-io-call-method  turtles--conn 'eval "ok"))))
    (turtles-stop))
  (should-not turtles--server)
  (should-not turtles--conn))


(ert-deftest turtles-test-message ()
  (turtles-start)

  (ert-with-message-capture messages
    (let* ((pid (turtles-io-call-method
                 turtles--conn
                 'eval
                 `(progn
                   (message "hello from turtles-test-message")
                   (emacs-pid))))
           (message (format "[PID %s] hello from turtles-test-message" pid)))
      (unless (member message (string-split messages "\n" 'omit-nulls))
        (error "message not found in %s" messages)))))

(require 'turtles-instance)
