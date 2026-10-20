;;; turtles-compat.el --- Backward-compatibility code -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Stephane Zermatten
;; and
;; Copyright (C) 1988, 1990, 1992, 1994-1995, 2001-2026 Free Software
;; Foundation, Inc.
;;
;; Some functions in this file have been backported from Emacs 29
;; term.el.

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
;; This package provides backward compatibility code for older
;; versions of Emacs.
;;

;;; Code:
(require 'term)

(defvar ansi-term-color-vector) ;; declared in term.el
(defvar ansi-color-bold-is-bright) ;; declared in ansi-color.el
(defvar term-ansi-current-bold) ;; declared in term.el
(defvar term-ansi-current-color) ;; declared in term.el
(defvar term-ansi-current-bg-color) ;; declared in term.el
(defvar term-ansi-current-underline) ;; declared in term.el
(defvar term-ansi-current-reverse) ;; declared in term.el
(defvar term-ansi-current-invisible) ;; declared in term.el
(defvar term-current-face) ;; declared in term.el

(defun turtles--term-ansi-color-code-as-hex (color)
  "Convert COLOR to hexadecimal string representation."
  ;; Backported from Emacs 29 term.el
  (cond
   ((< color 16) nil)
   ((>= color 256) (format "#%06x" (- color 256)))
   ((>= color 232) ;; Grayscale
    (format "#%06x" (* #x010101 (+ 8 (* 10 (- color 232))))))
   (t ;; 6x6x6 color cube
    (setq color (- color 16))
    (let ((res 0)
          (frac (* 6 6)))
      (while (<= 1 frac)
        (setq res (* res #x000100))
        (let ((color-num (mod (/ color frac) 6)))
          (unless (zerop color-num)
            (setq res (+ res #x37 (* #x28 color-num)))))
        (setq frac (/ frac 6)))
      (format "#%06x" res)))))

(defun turtles--term-color-as-hex (for-foreground)
  "Return the current ANSI color as a hexadecimal color string.
Use background color if FOR-FOREGROUND is nil, otherwise foreground color."
  ;; Backported from Emacs 29 term.el
  (let ((color (if for-foreground term-ansi-current-color
                 term-ansi-current-bg-color)))
    (or (turtles--term-ansi-color-code-as-hex (1- color))
        (progn
          (and (bound-and-true-p ansi-color-bold-is-bright)
               term-ansi-current-bold
               (<= 1 color 8)
               (setq color (+ color 8)))
          (if (and (boundp 'ansi-term-color-vector)
                   (< color (length ansi-term-color-vector)))
              (let ((face (elt ansi-term-color-vector color)))
                (if for-foreground
                    (face-foreground face nil 'default)
                  (face-background face nil 'default)))
            nil)))))

(defun turtles--term-handle-colors-list (parameters)
  "Parse ANSI SGR PARAMETERS and update `term-current-face'."
  ;; Backported from Emacs 29 term.el
  (setq parameters (or parameters '(0)))
  (while parameters
    (pcase (pop parameters)
      (1 (setq term-ansi-current-bold t))
      (2 (setq term-ansi-current-bold nil))
      (4 (setq term-ansi-current-underline t))
      (7 (setq term-ansi-current-reverse t))
      (8 (setq term-ansi-current-invisible t))
      (21 (setq term-ansi-current-bold nil))
      (22 (setq term-ansi-current-bold nil))
      (24 (setq term-ansi-current-underline nil))
      (27 (setq term-ansi-current-reverse nil))

      ;; Foreground
      ((and param (guard (<= 30 param 37)))
       (setq term-ansi-current-color (- param 29)))

      ;; Bright foreground
      ((and param (guard (<= 90 param 97)))
       (setq term-ansi-current-color (- param 81)))

      ;; Extended foreground
      (38
       (pcase (pop parameters)
         ;; 256 color
         (5 (if (setq term-ansi-current-color (pop parameters))
                (cl-incf term-ansi-current-color)
              (term-ansi-reset)))
         ;; Full 24-bit color
         (2 (cl-loop with color = (1+ 256)
                     for i from 16 downto 0 by 8
                     if (pop parameters)
                     do (setq color (+ color (ash it i)))
                     else return (term-ansi-reset)
                     finally
                     (if (> color (+ 1 256 #xFFFFFF))
                         (term-ansi-reset)
                       (setq term-ansi-current-color color))))
         (_ (term-ansi-reset))))

      ;; Reset foreground
      (39 (setq term-ansi-current-color 0))

      ;; Background
      ((and param (guard (<= 40 param 47)))
       (setq term-ansi-current-bg-color (- param 39)))

      ;; Bright background
      ((and param (guard (<= 100 param 107)))
       (setq term-ansi-current-bg-color (- param 91)))

      ;; Extended background
      (48
       (pcase (pop parameters)
         ;; 256 color
         (5 (if (setq term-ansi-current-bg-color (pop parameters))
                (cl-incf term-ansi-current-bg-color)
              (term-ansi-reset)))
         ;; Full 24-bit color
         (2 (cl-loop with color = (1+ 256)
                     for i from 16 downto 0 by 8
                     if (pop parameters)
                     do (setq color (+ color (ash it i)))
                     else return (term-ansi-reset)
                     finally
                     (if (> color (+ 1 256 #xFFFFFF))
                         (term-ansi-reset)
                       (setq term-ansi-current-bg-color color))))
         (_ (term-ansi-reset))))

      ;; Reset background
      (49 (setq term-ansi-current-bg-color 0))

      ;; 0 (Reset) or unknown
      (_ (term-ansi-reset))))

  (let (fg bg)
    (if term-ansi-current-invisible
        (setq bg (turtles--term-color-as-hex term-ansi-current-reverse)
              fg bg)
      (setq fg (turtles--term-color-as-hex t)
            bg (turtles--term-color-as-hex nil)))
    (setq term-current-face
          `( :foreground ,fg
             :background ,bg
             ,@(unless term-ansi-current-invisible
                 (list :inverse-video term-ansi-current-reverse)))))

  (setq term-current-face
        `(,term-current-face
          ,@(when term-ansi-current-bold '(term-bold))
          ,@(when term-ansi-current-underline '(term-underline)))))

(defun turtles--term-handle-ansi-escape (orig-fun proc params char)
  "Around advice for `term-handle-ansi-escape' to support 24-bit and 256 colors."
  (if (eq char ?m)
      (turtles--term-handle-colors-list params)
    (funcall orig-fun proc params char)))

(when (< emacs-major-version 29)
  (advice-add 'term-handle-ansi-escape :around #'turtles--term-handle-ansi-escape))

(provide 'turtles-compat)

;;; turtles-compat.el ends here
