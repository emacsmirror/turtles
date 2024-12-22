;;; turtles-term-eat-test.el --- Test integration with eat.el -*- lexical-binding: t -*-

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
(require 'eat)
(require 'turtles)
(require 'turtles-term-eat)

(turtles-definstance eat (:width 80 :height 20 :type 'eat)
  "A test instance that uses eat.")

(ert-deftest turtles-eat-hello-world ()
  (turtles-ert-test :instance 'eat)

  (ert-with-test-buffer ()
    (insert "hello, ")
    (insert (propertize "the " 'invisible t))
    (insert "world!\n")

    (turtles-with-grab-buffer ()
      (turtles-trim-buffer)

      (should (equal "hello, world!"
                     (buffer-string))))))
