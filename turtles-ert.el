;;; turtles-ert.el --- ERT integration for Turtles -*- lexical-binding: t -*-

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
;; This package contains ERT-specific functions for Turtles.
;;

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'turtles)
(require 'turtles-io)

(advice-add 'ert-run-test :around #'turtles-ert--around-ert-run-test)

(defvar turtles-ert--result nil
  "Result of running a test in another Emacs instance.")

(defun turtles-ert-test ()
  "Run the current test in another Emacs instance."
  (unless (turtles-client-p)
    (let* ((test (ert-running-test))
           (test-sym (when test (ert-test-name test)))
           (file-name (when test (ert-test-file-name test))))
      (unless test
        (error "Call turtles-ert-test from inside a ERT test."))
      (cl-assert test-sym)

      ;; TODO: if no file-name is available, transfer the test
      (unless file-name
        (error "No file available for %s" test-sym))

      (turtles-start)
      (setq turtles-ert--result
            (turtles-io-call-method-and-wait
             turtles--conn 'eval
             `(progn
                (load-library ,file-name)
                (let ((test (ert-get-test (quote ,test-sym))))
                  (ert-run-test test)
                  (ert-test-most-recent-result test)))))

      ;; ert-pass interrupt the server-side portion of the test. The
      ;; real result will be collected from turtles-ert--result by
      ;; turtles-ert--around-ert-run-test. What follows is the
      ;; client-side portion of the test only.
      (ert-pass))))

(defun turtles-ert--around-ert-run-test (func test)
  "Collect test results sent by another Emacs instance.

This function takes results set up by `turtles-ert-test' and puts
them into the local `ert-test' instance."
  (let ((turtles-ert--result nil))
    (funcall func test)
    (when turtles-ert--result
      (setf (ert-test-most-recent-result test) turtles-ert--result))))

(provide 'turtles-ert)

;;; turtles-ert.el ends here
