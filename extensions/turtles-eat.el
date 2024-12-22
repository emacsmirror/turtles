;;; turtles-eat.el --- Make turtles use eat as a terminal -*- lexical-binding: t -*-

;; Copyright (C) 2024 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 0.1snapshot
;; Package-Requires: ((turtles "0.1snapshot"))
;; Keywords: testing, unix
;; URL: http://github.com/szermatt/turtles

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
;; This package extends Turtles, providing it with a way of
;; interfacing with the eat package.
;;
;; This makes it possible to use eat as a terminal instead of term.el
;; and have truecolor support in older versions of Emacs.
;;

;;; Code:

(require 'cl-lib)
(require 'eat)
(require 'turtles-instance)

(cl-defmethod turtles--term-exec ((_type (eql 'eat)) cmdline _width _height)
  ;; Recompile the terminfo database once before even attempting to
  ;; start eat. This avoids issues with the precompiled database being
  ;; incompatible with the current system.
  (let ((stamp (expand-file-name ".recompiled" eat-term-terminfo-directory)))
    (unless (file-exists-p stamp)
      (eat-compile-terminfo)
      (with-temp-buffer (write-file stamp))))

  (eat-mode)
  ;; Force truecolor mode, no matter the display, so we can grab these
  ;; colors during tests.
  (setq-local eat-term-name "eat-truecolor")

  ;; Latency doesn't play well with tests. Turn it off.
  (setq-local eat-maximum-latency 0)
  (setq-local eat-minimum-latency 0)

  ;; Don't blink
  (setq-local eat-visible-cursor-type '(t nil nil))
  (setq-local eat-very-visible-cursor-type '(t nil nil))

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

(provide 'turtles-eat)

;;; turtles-eat.el ends here
