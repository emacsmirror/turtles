;;; turtles-term.el --- Turtles terminal using term.el -*- lexical-binding: t -*-

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
;; This package defines an adapter interface for turtles to
;; communicate with a terminal implementations.
;;

;;; Commentary:
;;
;; This package provides an implementation of the generic interface of
;; turtles-term.el that's based on the term.el package.
;;

;;; Code:

(require 'cl-lib)
(require 'term)
(require 'turtles-instance)

(defvar term-home-marker) ;; declared in term.el
(defvar term-width) ;; declared in term.el
(defvar term-height) ;; declared in term.el

(cl-defmethod turtles--term-exec ((_type (eql 'term)) cmdline width height)
  (term-mode)
  (setq-local term-width width)
  (setq-local term-height height)
  (term-exec (current-buffer) (buffer-name) (car cmdline) nil (cdr cmdline))
  (term-char-mode))

(cl-defmethod turtles--term-truecolor-p ((_type (eql 'term)))
  (>= emacs-major-version 29))

(cl-defmethod turtles--term-resize ((_type (eql 'term)) w h)
  (unless (and (= term-width w) (= term-height h))
    (set-process-window-size (get-buffer-process (current-buffer)) h w)
    (term-reset-size h w)

    t))

(cl-defmethod turtles--term-screen-string ((_type (eql 'term)))
  (turtles--term-substring-with-properties
   term-home-marker (point-max) '((font-lock-face . face))))

(provide 'turtles-term)
