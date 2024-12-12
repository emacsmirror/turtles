;; turtles-ert-test.el --- Test turtles-ert.el -*- lexical-binding: t -*-

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
(require 'cl-lib)

(require 'turtles)
(require 'turtles-io)
(require 'turtles-ert)

(ert-deftest turtles-ert-test-smoke ()
  (if (turtles-client-p)
      (progn
        (should (equal 1 1)))
    (let* ((test (ert-running-test))
           (test-sym (ert-test-name test))
           (file-name (ert-test-file-name test)))
      (cl-assert test)
      (cl-assert test-sym)
      (cl-assert file-name)

      (turtles-start)
      (unwind-protect
          (setq turtles-ert--result
                (turtles-io-call-method-and-wait
                 turtles--conn 'eval
                 `(progn
                    (load-library ,file-name)
                    (let ((test (ert-get-test (quote ,test-sym))))
                      (ert-run-test test)
                      (ert-test-most-recent-result test)))))))))
