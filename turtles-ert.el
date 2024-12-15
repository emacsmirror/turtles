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
(require 'ert-x)
(require 'cl-lib)

(require 'turtles)
(require 'turtles-io)

(advice-add 'ert-run-test :around #'turtles-ert--around-ert-run-test)

(defvar turtles-ert--result nil
  "Result of running a test in another Emacs instance.")

(defun turtles-ert-test ()
  "Run the current test in another Emacs instance."
  (unless (turtles-client-p)
    (let* ((test (ert-running-test))
           (test-sym (when test (ert-test-name test)))
           (file-name (when test (ert-test-file-name test))))
      (unless test
        (error "Call turtles-ert-test from inside a ERT test."))
      (cl-assert test-sym)

      ;; TODO: if no file-name is available, transfer the test
      (unless file-name
        (error "No file available for %s" test-sym))

      (turtles-start)
      (setq turtles-ert--result
            (turtles-io-call-method
             turtles--conn 'eval
             `(progn
                (load ,file-name nil 'nomessage 'nosuffix)
                (clear-minibuffer-message)
                (menu-bar-mode -1)
                (let ((test (ert-get-test (quote ,test-sym))))
                  (ert-run-test test)
                  (ert-test-most-recent-result test)))))

      ;; ert-pass interrupt the server-side portion of the test. The
      ;; real result will be collected from turtles-ert--result by
      ;; turtles-ert--around-ert-run-test. What follows is the
      ;; client-side portion of the test only.
      (ert-pass))))

(defun turtles-ert--around-ert-run-test (func test)
  "Collect test results sent by another Emacs instance.

This function takes results set up by `turtles-ert-test' and puts
them into the local `ert-test' instance."
  (let ((turtles-ert--result nil))
    (funcall func test)
    (when turtles-ert--result
      (setf (ert-test-most-recent-result test) turtles-ert--result))))

(cl-defun turtles-to-string (&key (name "grab")
                                   frame win buf minibuffer
                                   faces region point (trim t))
  "Grab a section of the terminal and return the result as a string.

With no arguments, this function renders the current buffer in
the turtles frame, and returns the result as a string.

The rendered buffer is grabbed into an ERT test buffer with the
name \"grab\". When running interactively, ERT keeps such buffers
when tests fail so they can be checked out. Specify the keyword
argument NAME to modify the name of the test buffer.

The following keyword arguments modify what is grabbed:

  - The key argument BUF specifies a buffer to capture. It
    defaults to the current buffer. The buffer is installed into
    the single window of `turtles-frame', rendered, then
    grabbed.

  - The key argument WIN specifies a window to grab. The window
    must be part of `turtles-frame'.

  - Set the key argument MINIBUFFER to t to capture the content
    of the minibuffer window of `turtles-frame'.

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
    need to be of the same size. See also `turtles-mark-region'.

  - The key argument FACES makes a specific set of faces visible
    in the returned string. Pass an alist with the symbols of the
    faces you want to highlight as key and either one string
    composed of opening and closing strings of the same length,
    such as \"[]\" or \"/**/\", to mark the beginning and end of
    the region. Alternatively, you can also pass a list made up
    of two strings, the opening and closing string, which then
    don't need to be of the same size. See also
    `turtles-mark-text-with-faces'"
  (let ((calling-buf (current-buffer)))
    (ert-with-test-buffer (:name name)
      (turtles--internal-grab
       frame win buf calling-buf minibuffer faces)
      (when region
        (turtles-mark-region (if (consp region) (car region) region)
                              (if (consp region) (nth 1 region))))
      (when point
        (insert point))
      (turtles-mark-text-with-faces (turtles-ert--filter-faces-for-mark faces))
      (when trim
        (turtles-trim-buffer))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmacro turtles-with-grab-buffer ((&key (name "grab")
                                              frame win buf minibuffer
                                              faces)
                                        &rest body)
  "Grab a section of the terminal and store it into a test buffer.

With no arguments, this function renders the current buffer in
the turtles frame into an ERT test buffer and executes BODY.

The ERT test buffer with the name \"grab\". When running
interactively, ERT keeps such buffers when tests fail so they can
be checked out. Specify the keyword argument NAME to modify the
name of the test buffer.

The garbbed buffer contains a textual representation of the frame
or window captured on the turtles frame. When grabbing a buffer
or window, the point and region will be grabbed as well.
Additionally, unless FACES is specified, captured colors are
available as overlay colors, within the limits of the turtles
terminal, usually limited to 256 colors.

More keyword arguments can be specified in parentheses, before
BODY:

  - The key argument BUF specifies a buffer to capture. It
    defaults to the current buffer. The buffer is installed into
    the single window of `turtles-frame', rendered, then
    grabbed.

  - The key argument WIN specifies a window to grab. The window
    must be part of `turtles-frame'.

  - Set the key argument MINIBUFFER to t to capture the content
    of the minibuffer window of `turtles-frame'.

  - Set the key argument FRAME to t to capture the whole frame.

  - The key argument FACES asks for a specific set of faces to
    be detected and grabbed. They'll be available as face
    symbols set to the properties \\='face.

    If necessary, faces can be made easier to test with text
    comparison with `turtles-mark-text-with-faces'.

    Note that colors won't be available in the grabbed buffer
    content when FACES is specified."
  (declare (indent 1))
  (let ((calling-buf (make-symbol "calling-buf"))
        (faces-var (make-symbol "faces")))
    `(let ((,calling-buf (current-buffer))
           (,faces-var ,faces))
       (ert-with-test-buffer (:name ,name)
         (turtles--internal-grab
          ,frame ,win ,buf ,calling-buf ,minibuffer ,faces-var)
         (turtles-mark-text-with-faces (turtles-ert--filter-faces-for-mark ,faces-var))

         ,@body))))

(defmacro turtles-read-from-minibuffer (read &rest body)
  "Run BODY while executing READ.

READ is a form that reads from the minibuffer and return the
result.

BODY is executed while READ is waiting for minibuffer input with
the minibuffer active. Input can be provided by calling
`execute-kbd-macro'. BODY must eventually either signal an error
or exit the minibuffer.

This macro allows mixing `execute-kbd-macro' and commands
manipulating minibuffer with grab commands such as
`turtles-to-string' and `turtles-with-grab-buffer'. `should'
can also be called directly on BODY.

This is provided here as a replacement to `ert-simulate-keys', as
the approach taken by `ert-simulate-keys' doesn't allow grabbing
intermediate states. This is because Emacs won't redisplay as
long as there's pending input.

Return whatever READ eventually evaluates to."
  (declare (indent 1))
  (let ((mb-result-var (make-symbol "mb-result")))
    `(progn
       (run-with-timer
        0 nil
        (lambda ()
          (setq ,mb-result-var (progn ,read))))
       (run-with-timer
        0 nil
        (lambda ()
          (progn ,@body)
          (when (active-minibuffer-window)
            (error "Minibuffer still active at end of body form"))))
       (sleep-for 0.01)
       ,mb-result-var)))

(defun turtles--internal-grab (frame win buf calling-buf minibuffer grab-faces)
  "Internal macro implementation for grabbing into the current buffer.

Do not call this function outside of this file."
  (let ((cur (current-buffer))
        (grab-faces (turtles-ert--filter-faces-for-grab grab-faces)))
    (cond
     (buf (turtles-grab-buffer-into buf cur grab-faces))
     (win (turtles-grab-window-into win cur grab-faces))
     (minibuffer (turtles-grab-window-into (active-minibuffer-window) cur grab-faces))
     (frame (turtles-grab-frame-into cur grab-faces))
     (t (turtles-grab-buffer-into calling-buf cur grab-faces)))))

(defun turtles-ert--filter-faces-for-grab (faces)
  "Filter FACES t pass to `turtles-grab-buffer-into'"
  (mapcar (lambda (c) (if (consp c) (car c) c)) faces))

(defun turtles-ert--filter-faces-for-mark (faces)
  (delq nil (mapcar (lambda (c) (if (consp c) c)) faces)))

(provide 'turtles-ert)

;;; turtles-ert.el ends here
