;;; turtles-term-eat.el --- Turtles uses eat as terminal -*- lexical-binding: t -*-

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
;; This package provides an implementation of the generic interface of
;; turtles-term.el that's based on the eat package.
;;

;;; Code:

(require 'cl-lib)
(require 'eat)
(require 'turtles-term)

(cl-defmethod turtles--term-exec ((_type (eql 'eat)) cmdline _width _height)
  (eat-mode)
  ;; Force truecolor mode, no matter the display, so we can grab these
  ;; colors during tests.
  (setq-local eat-term-name "eat-truecolor")

  ;; Latency doesn't play well with tests. Turn it off.
  (setq-local eat-maximum-latency 0)
  (setq-local eat-minimum-latency 0)

  (eat-exec (current-buffer) (buffer-name) (car cmdline) nil (cdr cmdline)))

(cl-defmethod turtles--term-truecolor-p ((_type (eql 'eat)))
  t)

(cl-defmethod turtles--term-resize ((_type (eql 'eat)) width height)
  (let ((size (eat-term-size eat-terminal)))
    (when (or (/= width (car size))
              (/= height (cdr size)))
      (let ((inhibit-read-only t))
        (eat-term-resize eat-terminal width height)
        (eat-term-redisplay eat-terminal))

      t)))

(cl-defmethod turtles--term-screen-string ((_type (eql 'eat)))
  (turtles--term-substring-with-properties
          (eat-term-beginning eat-terminal)
          (eat-term-end eat-terminal)
          '((font-lock-face . face)
            (face . face))))

(provide 'turtles-term-eat)
