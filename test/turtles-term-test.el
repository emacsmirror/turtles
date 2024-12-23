;;; turtles-term-test.el --- Test integration with term.el -*- lexical-binding: t -*-

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
(require 'term)
(require 'turtles)
(require 'turtles-term)

(ert-deftest turtles-term-truecolor ()
  (skip-unless (>= emacs-major-version 29))
  ;; Truecolor in term.el became available in Emacs 29.1. Before that,
  ;; term was limited to just 16 (Emacs 28) and even 8 (Emacs 26)
  ;; colors.

  (turtles-ert-test)

  (should (equal 16777216 (display-color-cells)))

  (ert-with-test-buffer ()
    (insert (propertize "yellow" 'face '(:foreground "#faf32c" :background "#3a3913")))
    (insert " ")
    (insert (propertize "submarine" 'face '(:foreground "#276ce2" :background "#0c1526")))
    (insert "\n")

    (turtles-with-grab-buffer ()
      (turtles-trim-buffer)

      (goto-char (point-min))
      (search-forward "yellow")
      (goto-char (match-beginning 0))
      (should (string-equal-ignore-case "#faf32c" (foreground-color-at-point)))
      (should (string-equal-ignore-case "#3a3913" (background-color-at-point)))

      (search-forward "submarine")
      (goto-char (match-beginning 0))
      (should (string-equal-ignore-case "#276ce2" (foreground-color-at-point)))
      (should (string-equal-ignore-case "#0c1526" (background-color-at-point))))))
