;;; turtles-term.el --- Generic terminal interface -*- lexical-binding: t -*-

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

;;; Code:
(require 'cl-lib)

(cl-defgeneric turtles--term-exec (type cmdline width height)
  "Execute CMDLINE in a terminal of the TYPE in the current buffer.

The terminal size is set to WIDTH x HEIGHT.")

(cl-defgeneric turtles--term-resize (type width height)
  "Set the size of the terminal in the current buffer.

TYPE specifies the terminal type. It must be the same as what was
passed to `turtles--term-exec'.

This function resizes the terminal to WIDTH x HEIGHT, if needed and return
non-nil. If the terminal size is already correct, return nil.")

(cl-defgeneric turtles--term-screen-string (type)
  "Return a string containing the current buffer terminal screen.

TYPE specifies the terminal type. It must be the same as what was
passed to `turtles--term-exec'.")


(defun turtles--term-substring-with-properties (start end prop-alist)
  "Take a string from a region of the current buffer.

This function takes the string at the region START to END from
the current buffer and copies only the properties listed in
PROP-ALIST to the resulting string.

PROP-ALIST is a list of source properties to dest properties."
  (let ((str (buffer-substring-no-properties start end)))
    (dolist (prop-cell prop-alist)
      (turtles--term-copy-property
       (current-buffer) start end str 0 (car prop-cell) (cdr prop-cell)))

    str))

(defun turtles--term-copy-property (src start-src end-src dest start-dest prop-src prop-dest)
  "Copy a single property from SRC to DEST.

START-SRC and END-SRC defines the source range in SRC.

DEST is the destination object. The destination range start at
START-DEST and is of the same length as the source range.

PROP-SRC is the property from SRC to copy and PROP-DEST is the
property to set in DEST.

SRC and DEST can be a string or a buffer."
  (let ((pos-src start-src)
        (diff (- start-dest start-src))
        next-pos-src)
    (while (< pos-src end-src)
      (let ((val (get-text-property pos-src prop-src src)))
        (setq next-pos-src (next-single-property-change pos-src prop-src src end-src))
        (when val
          (add-text-properties
           (+ pos-src diff) (+ next-pos-src diff) (list prop-dest val) dest))
        (setq pos-src next-pos-src)))))

(provide 'turtles-term)
