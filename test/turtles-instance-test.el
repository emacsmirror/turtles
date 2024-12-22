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

(turtles-definstance turtles--restart ()
  "A private test instance to test restart.")

(turtles-definstance turtles--132x43 (:width 132 :height 43)
  "A private test instance with a larger frame.")

(ert-deftest turtles-instance-restart ()
  (turtles-start-server)
  (should turtles--server)
  (should (turtles-io-server-live-p turtles--server))

  (let ((inst (turtles-get-instance 'turtles--restart))
        buf proc)
    (should inst)
    (should (eq inst (turtles-stop-instance inst)))
    (should (eq inst (turtles-start-instance inst)))
    (should (turtles-instance-live-p inst))

    (setq buf (turtles-instance-term-buf inst))
    (should (buffer-live-p buf))
    (should (process-live-p (get-buffer-process buf)))

    (setq proc (turtles-io-conn-proc (turtles-instance-conn inst)))
    (should (process-live-p proc))

    (should (equal "ok" (turtles-instance-eval inst "ok")))

    (turtles-stop-instance inst)

    (should-not (turtles-instance-live-p inst))
    (should-not (buffer-live-p buf))
    (should-not (process-live-p proc))))

(ert-deftest turtles-instance-message ()
  (let ((inst (turtles-start-instance 'default)))
    (let ((inhibit-message t))
      (ert-with-message-capture messages
        (turtles-instance-eval inst
         '(let ((turtles-send-messages-upstream t))
            (message "hello from turtles-test-message")))
        (let ((message "[default] hello from turtles-test-message"))
          (unless (member message (string-split messages "\n" 'omit-nulls))
            (error "message not found in %s" messages)))))))

(ert-deftest turtles-instance-default-size ()
  (let ((inst (turtles-start-instance 'default)))
    (with-current-buffer (turtles-instance-term-buf inst)
      (should (equal 80 term-width))
      (should (equal 20 term-height)))))

(ert-deftest turtles-instance-turtles--132x43-size ()
  (let ((inst (turtles-start-instance 'turtles--132x43)))
    (with-current-buffer (turtles-instance-term-buf inst)
      (should (equal 132 term-width))
      (should (equal 43 term-height)))))

(ert-deftest turtles-instance-unreadable-buffer ()
  (let ((inst (turtles-start-instance 'default)))
    (should
     (equal '(turtles-buffer :name "*scratch*" :instance default)
            (turtles-instance-eval inst
             '(get-scratch-buffer-create))))))

(ert-deftest turtles-instance-customize-send-messages-upstream ()
  (let ((inst (turtles-start-instance 'default))
        (original-value turtles-send-messages-upstream)
        (inhibit-message t)
        conn)
    (setq conn (turtles-instance-conn inst))

    (unwind-protect
        (ert-with-message-capture messages
          (custom-set-variables '(turtles-send-messages-upstream t now))
          (should turtles-send-messages-upstream)
          (should (turtles-instance-eval inst 'turtles-send-messages-upstream))
          (turtles-instance-eval inst '(message "message1, sent upstream"))

          (custom-set-variables '(turtles-send-messages-upstream nil now))
          (should-not turtles-send-messages-upstream)
          (should-not (turtles-io-call-method conn 'eval 'turtles-send-messages-upstream))
          (turtles-instance-eval inst '(message "message2, not sent upstream"))

          (should (member "[default] message1, sent upstream"
                          (string-split messages "\n")))
          (should-not (member "[default] message2, not sent upstream"
                              (string-split messages "\n")))

          (custom-set-variables `(turtles-send-messages-upstream ,original-value now)))
      (setq turtles-send-messages-upstream original-value))))

(require 'turtles-instance)
