;; turtles.el --- Test turtles.el -*- lexical-binding: t -*-

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

(require 'ert)
(require 'ert-x)

(require 'turtles)
(require 'turtles-io)
(require 'turtles-ert)

(ert-deftest turtles-start-stop ()
  (unwind-protect
      (progn
        (turtles-start)
        (should turtles--server)
        (should turtles--conn)
        (should (equal "ok" (turtles-io-call-method-and-wait turtles--conn 'eval "ok"))))
    (turtles-stop))
  (should-not turtles--server)
  (should-not turtles--conn)
  (should-not (get-buffer turtles-buffer-name)))

(ert-deftest turtles-grab-frame-into ()
  (turtles-ert-test)

  (with-current-buffer (get-scratch-buffer-create)
    (select-window (display-buffer (current-buffer) '(display-buffer-full-frame . nil)))
    (insert "De Chelonian Mobile")
    (with-temp-buffer
      (turtles-grab-frame-into (current-buffer))
      (goto-char (point-min))
      (should (search-forward "De Chelonian Mobile")))))
