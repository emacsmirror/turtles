;;; turtles.el --- Screen-grabbing test utility -*- lexical-binding: t -*-

;; Copyright (C) 2024 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 0.1snapshot
;; Package-Requires: ((emacs "29.1"))
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
;; This package contains utilities for testing Emacs appearance in a
;; terminal.
;;

;;; Code:

(require 'term)
(require 'server)
(require 'turtles-io)

(defvar turtles--server nil)
(defvar turtles--conn nil)
(defvar turtles--file-name (or (when load-file-name
                                 (expand-file-name load-file-name default-directory))
                               (buffer-file-name)))

(defun turtles-start ()
  (interactive)
  (unless (turtles-io-server-live-p turtles--server)
    (server-ensure-safe-dir server-socket-dir)
    (setq turtles--server (turtles-io-server
                           (expand-file-name (format "turtles-%s" (emacs-pid))
                                             server-socket-dir))))

  (let ((buf (get-buffer-create " *turtles-term*")))
    (unless (and (turtles-io-conn-live-p turtles--conn)
                 (term-check-proc buf))
      (mapc (lambda (c) (delete-process (turtles-io-conn-proc c)))
            (turtles-io-server-connections turtles--server))
      (setq turtles--conn nil)

      (with-current-buffer buf (term-mode))
      (term-exec
       buf
       "*turtles*"
       (expand-file-name invocation-name invocation-directory)
       nil
       (append '("-nw" "-Q")
               (turtles--dirs-from-load-path)
               `("-l" ,turtles--file-name "--" "boo")))

      (setf (turtles-io-server-on-new-connection turtles--server)
            (lambda (conn)
              (setf turtles--conn conn)
              (setf (alist-get 'term-buffer (turtles-io-conn-alist conn)) buf)
              (setf (turtles-io-server-on-new-connection turtles--server) nil)))

      (with-current-buffer buf
        (term-send-raw-string
         (format "\033xturtles--launch\n%s\n"
                 (turtles-io-server-socket turtles--server))))

      (turtles-io-wait-for 5 "Turtles Emacs failed to connect"
                           (lambda () turtles--conn)))))

(defun turtles--dirs-from-load-path ()
  (let ((args nil))
    (dolist (path load-path)
      (push "-L" args)
      (push path args))
    (nreverse args)))

(defun turtles--launch (socket)
  (interactive "F")
  (turtles-io-connect socket))

(provide 'turtles)
