;; turtles-examples-test.el --- Example tests using turtles. -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025 Stephane Zermatten

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
(require 'easymenu)
(require 'ert)
(require 'ert-x)
(require 'hideshow)
(require 'isearch)

(require 'turtles)
(require 'mule-util) ;; for truncate-string-ellipsis

;; Snippet shown in README.md
(turtles-ert-deftest turtles-examples-hello-world ()
  ;; This test runs in the secondary instance.
  (ert-with-test-buffer ()
    (insert "hello, ") ; Fill in the buffer
    (insert (propertize "the " 'invisible t))
    (insert "world!\n")

    (turtles-with-grab-buffer () ; Grab current buffer content
      (should (equal "hello, world!"
                     (buffer-string))))))

(turtles-ert-deftest turtles-examples-test-hideshow ()
  (let ((truncate-string-ellipsis "..."))
  (ert-with-test-buffer ()
    (insert "(defun test-1 ()\n")
    (insert " (message \"test, the first\"))\n")
    (insert "(defun test-2 ()\n")
    (insert " (message \"test, the second\"))\n")
    (insert "(defun test-3 ()\n")
    (insert " (message \"test, the third\"))\n")

    (emacs-lisp-mode)
    (hs-minor-mode)

    (goto-char (point-min))
    (search-forward "test-2")
    (hs-hide-block)
    (turtles-with-grab-buffer (:name "hide test-2")
      (should (equal (concat
                      "(defun test-1 ()\n"
                      " (message \"test, the first\"))\n"
                      "(defun test-2 ()...)\n"
                      "(defun test-3 ()\n"
                      " (message \"test, the third\"))")
                     (buffer-substring-no-properties (point-min) (point-max)))))

    (hs-hide-all)
    (turtles-with-grab-buffer (:name "hide all")
      (should (equal (concat
                      "(defun test-1 ()...)\n"
                      "(defun test-2 ()...)\n"
                      "(defun test-3 ()...)")
                     (buffer-string)))))))

(turtles-ert-deftest turtles-examples-test-completing-read ()
  (ert-with-test-buffer ()
    (let ((completing-read-function #'completing-read-default))
      (turtles-with-minibuffer
          (should
           (equal "Choice B"
                  (completing-read "Choose: " '("Choice A" "Choice B") nil t)))

        (turtles-with-grab-buffer (:name "initial prompt" :point "<>")
          (should (equal "Choose: <>" (buffer-string))))

        (execute-kbd-macro "Ch")
        (minibuffer-complete)
        (turtles-with-grab-buffer (:name "completion" :point "<>")
          (should (equal "Choose: Choice <>" (buffer-string))))

        (execute-kbd-macro "B")))))

(turtles-ert-deftest turtles-examples-test-isearch ()
  (ert-with-test-buffer ()
    (let ((testbuf (current-buffer)))
      (select-window (display-buffer testbuf))
      (delete-other-windows)

      (insert "Baa, baa, black sheep, have you any wool?")
      (goto-char (point-min))

      (turtles-with-minibuffer
          (isearch-forward)

        :keys "baa"
        (turtles-with-grab-buffer (:minibuffer t)
          (should (equal "I-search: baa" (buffer-string))))
        (turtles-with-grab-buffer (:buf testbuf :name "match 1" :faces '((isearch "[]")))
          (should (equal "[Baa], baa, black sheep, have you any wool?"
                         (buffer-string))))

        :keys "\C-s"
        (turtles-with-grab-buffer (:buf testbuf :name "match 2" :faces '((isearch "[]")))
          (should (equal "Baa, [baa], black sheep, have you any wool?"
                         (buffer-string))))

        (isearch-done))

      (turtles-with-grab-buffer (:name "final position" :point "<>")
        (should (equal "Baa, baa<>, black sheep, have you any wool?"
                       (buffer-string)))))))

(turtles-ert-deftest turtles-example-menu ()
  (skip-unless (>= emacs-major-version 28))
  (menu-bar-mode)

  (ert-with-test-buffer ()
    (turtles-example-mode)
    (pop-to-buffer (current-buffer))
    (turtles-interaction
      ;; Find and click on menu in the menu bar
      :mouse-ev 'mouse-1 (turtles-coords
                          "Turtle!" :menu-bar t)
      (turtles-with-grab-buffer (:menu t)
        (should
         (equal
          (concat
           " Turtle! >\n"
           " Swim\n"
           " Eat Lettuce\n"
           " Hide in Shell")
          (buffer-string))))

      ;; Click on item to select it
      :mouse-ev 'mouse-1 (turtles-coords
                          "Swim" :menu t))

    ;; Swim was executed
    (should (equal "weee!" (buffer-string)))))

(turtles-ert-deftest turtles-example-mode-line-menu ()
  (skip-unless (>= emacs-major-version 28))
  (ert-with-test-buffer ()
    ;; Keep the buffer name short, on the mode won't appear on the
    ;; mode line.
    (rename-buffer "turtle" 'unique)

    (turtles-example-mode)
    (pop-to-buffer (current-buffer))
    (turtles-interaction
      ;; Find and press mouse button on menu in the mode line
      :mouse-ev 'down-mouse-1 (turtles-coords
                               "Turtle!" :mode-line (current-buffer))
      (turtles-with-grab-buffer (:menu t)
        (should
         (equal
          (concat
           " Turtle! Mode >\n"
           " Swim\n"
           " Eat Lettuce\n"
           " Hide in Shell")
          (buffer-string))))

      ;; Release button over item to select it.
      :mouse-ev 'up-mouse-1 (turtles-coords
                             "Eat Lettuce" :menu t))

    ;; Eat Lettuce was executed
    (should (equal "*chomp* *chomp*" (buffer-string)))))

(define-derived-mode turtles-example-mode fundamental-mode "Turtle!"
  "Sample major mode with a menu.")

(easy-menu-define turtles-example-mode-menu turtles-example-mode-map
  "Menu for `turtles-example-mode'."
  '("Turtle!"
    ["Swim" (lambda () (interactive) (insert "weee!"))]
    ["Eat Lettuce" (lambda () (interactive) (insert "*chomp* *chomp*"))]
    ["Hide in Shell" (lambda () (interactive) (insert "..."))]))
