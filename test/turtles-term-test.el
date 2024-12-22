;;; turtles-term-test.el --- Test turtles-term.el -*- lexical-binding: t -*-

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
(require 'turtles-term)
(require 'turtles)

(ert-deftest turtles-term-substring-with-properties ()
  (ert-with-test-buffer ()
    (let ((source (current-buffer)))
      (let ((inhibit-read-only t))
        (insert (propertize "This buffer " 'read-only t))
        (insert (propertize "is" 'font-lock-face '(:foreground "purple")))
        (insert " ")
        (insert (propertize "in" 'face '(:foreground "yellow")))
        (insert " ")
        (insert (propertize "full"
                            'font-lock-face '(:foreground "red")
                            'cursor-face '(:background "cyan")))
        (insert " ")
        (insert (propertize "color" 'face '(:foreground "blue"))))

      (let ((str (turtles--term-substring-with-properties
                  ;; start
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "buffer")
                    (match-beginning 0))
                  ;; end
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "full"))
                  '((face . face) (font-lock-face . face)))))

        (should (equal "buffer is in full" str))
        (should (equal '((0 7 nil)
                         (7 9 (face (:foreground "purple"))) ;; is
                         (9 10 nil)
                         (10 12 (face (:foreground "yellow"))) ;; in
                         (12 13 nil)
                         (13 17 (face (:foreground "red")))) ;; full
                       (object-intervals str)))))))
