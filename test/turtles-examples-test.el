;; turtles-examples-test.el --- Example tests using turtles. -*- lexical-binding: t -*-

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

(require 'turtles)

;; Snippet shown in README.md
(ert-deftest turtles-hello-world ()
   ;; Start a secondary Emacs instance
  (turtles-ert-test)
  
  ;; From this point, everything runs in the secondary instance.
  (ert-with-test-buffer ()
    (insert "hello, ") ; Fill in the buffer
    (insert (propertize "the " 'invisible t))
    (insert "world!\n")

    (turtles-with-grab-buffer () ; Grab current buffer content
      (should (equal "hello, world!"
                     (buffer-string))))))

