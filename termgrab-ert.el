;;; termgrab-ert.el --- Screen-grabbing macros for ERT -*- lexical-binding: t -*-

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
;; This package contains ERT-specific convenience macros for working with
;; termgrab.
;;

;;; Code:

(require 'termgrab)
(require 'ert-x)
(require 'cl-lib)

(cl-defun termgrab-to-string (&key (name "grab")
                                   frame win buf minibuffer
                                   faces region point (trim t))
  "Grab a section of the terminal and return the result as a string.

With no arguments, this function renders the current buffer in
the termgrab frame, and returns the result as a string.

The rendered buffer is grabbed into an ERT test buffer with the
name \"grab\". When running interactively, ERT keeps such buffers
when tests fail so they can be checked out. Specify the keyword
argument NAME to modify the name of the test buffer.

The following keyword arguments modify what is grabbed:

  - The key argument BUF specifies a buffer to capture. It
    defaults to the current buffer. The buffer is installed into
    the single window of `termgrab-frame', rendered, then
    grabbed. 

  - The key argument WIN specifies a window to grab. The window
    must be part of `termgrab-frame'.

  - Set the key argument MINIBUFFER to t to capture the content
    of the minibuffer window of `termgrab-frame'.

  - Set the key argument FRAME to t to capture the whole frame.

The following keyword arguments post-process what was grabbed:

  - Set the key argument TRIM to nil to not trim the newlines
    at the end of the grabbed string. Without trimming, there
    is one newline per line of the grabbed window, even if
    the buffer content is shorter.

  - Pass a string to the key argument POINT to insert at point,
    so that position is visible in the returned string.

  - The key argument REGION makes the active region visible in
    the returned string. Pass a string composed of opening and
    closing strings of the same length, such as \"[]\" or
    \"/**/\", to mark the beginning and end of the region.
    Alternatively, you can also pass a list made up of two
    strings, the opening and closing string, which then don't
    need to be of the same size. See also `termgrab-mark-region'.

  - The key argument FACES makes a specific set of faces visible
    in the returned string. Pass an alist with the symbols of the
    faces you want to highlight as key and either one string
    composed of opening and closing strings of the same length,
    such as \"[]\" or \"/**/\", to mark the beginning and end of
    the region. Alternatively, you can also pass a list made up
    of two strings, the opening and closing string, which then
    don't need to be of the same size. See also
    `termgrab-mark-text-with-faces'"
  (let ((calling-buf (current-buffer)))
    (ert-with-test-buffer (:name name)
      (termgrab--internal-grab
       frame win buf calling-buf minibuffer (mapcar #'car faces))
      (when region
        (termgrab-mark-region (if (consp region) (car region) region)
                              (if (consp region) (nth 1 region))))
      (when point
        (insert point))
      (when faces (termgrab-mark-text-with-faces faces))
      (if trim
          (string-trim-right
           (buffer-substring-no-properties (point-min) (point-max)))
        (buffer-substring-no-properties (point-min) (point-max))))))

(cl-defmacro termgrab-with-grab-buffer ((&key (name "grab")
                                              frame win buf minibuffer
                                              faces)
                                        &rest body)
  "Grab a section of the terminal and store it into a test buffer.

With no arguments, this function renders the current buffer in
the termgrab frame into an ERT test buffer and executes BODY.

The ERT test buffer with the name \"grab\". When running
interactively, ERT keeps such buffers when tests fail so they can
be checked out. Specify the keyword argument NAME to modify the
name of the test buffer.

The garbbed buffer contains a textual representation of the frame
or window captured on the termgrab frame. When grabbing a buffer
or window, the point and region will be grabbed as well.
Additionally, unless FACES is specified, captured colors are
available as overlay colors, within the limits of the termgrab
terminal, usually limited to 256 colors.

More keyword arguments can be specified in parentheses, before
BODY:

  - The key argument BUF specifies a buffer to capture. It
    defaults to the current buffer. The buffer is installed into
    the single window of `termgrab-frame', rendered, then
    grabbed.

  - The key argument WIN specifies a window to grab. The window
    must be part of `termgrab-frame'.

  - Set the key argument MINIBUFFER to t to capture the content
    of the minibuffer window of `termgrab-frame'.

  - Set the key argument FRAME to t to capture the whole frame.

  - The key argument FACES asks for a specific set of faces to
    be detected and grabbed. They'll be available as face 
    symbols set to the properties \\='face.

    If necessary, faces can be made easier to test with text
    comparison with `termgrab-mark-text-with-faces'.

    Note that colors won't be available in the grabbed buffer
    content when FACES is specified."
  (declare (indent 1))
  (let ((calling-buf (make-symbol "calling-buf")))
    `(let ((,calling-buf (current-buffer)))
       (ert-with-test-buffer (:name ,name)
         (termgrab--internal-grab
          ,frame ,win ,buf ,calling-buf ,minibuffer ,faces)))))

(defun termgrab--internal-grab (frame win buf calling-buf minibuffer grab-faces)
  "Internal macro implementation for grabbing into the current buffer.

Do not call this function outside of this file."
  (let ((cur (current-buffer)))
  (cond
   (buf (termgrab-grab-buffer-into buf cur grab-faces))
   (win (termgrab-grab-window-into win cur grab-faces))
   (minibuffer (termgrab-grab-window-into (termgrab-minibuffer-window) cur grab-faces))
   (frame (termgrab-grab-frame-into cur grab-faces))
   (t (termgrab-grab-buffer-into calling-buf cur grab-faces)))))

(provide 'termgrab-ert)

;;; termgrab-ert.el ends here
