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

(defvar turtles-ert--result nil)

(defun turtles-ert--around-ert-run-test (func test)
  (let ((turtles-ert--result nil))
    (funcall func test)
    (when turtles-ert--result
      (setf (ert-test-most-recent-result test) turtles-ert--result))))

(provide 'turtles-ert)

;;; turtles-ert.el ends here
