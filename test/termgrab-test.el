;; termgrab-test.el --- Test termgrab.el -*- lexical-binding: t -*-

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

(require 'termgrab)

(ert-deftest termgrab-test-smoke ()
  (unwind-protect
      (progn
        (termgrab-start-server)
        (should termgrab-server-proc)
        (should termgrab-frame)
        (should (string-prefix-p
                 "grab: 1 windows"
                 (with-temp-buffer
                   (termgrab--tmux termgrab-server-proc (current-buffer) "list-sessions")
                   (buffer-string))))
        (let ((grabbed (termgrab-grab-to-string)))
          (should (stringp grabbed))
          (should (string-match-p "scratch" grabbed))))
    (termgrab-stop-server))
  (should-not termgrab-server-proc))

;;; termgrab-test.el ends here
