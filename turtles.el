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

(defvar term-home-marker) ;; declared in term.el

(defvar turtles--server nil)
(defvar turtles--conn nil)
(defvar turtles--file-name (or (when load-file-name
                                 (expand-file-name load-file-name default-directory))
                               (buffer-file-name)))
(defconst turtles-buffer-name " *turtles-term*")

(defun turtles-client-p ()
  (and turtles--conn (not turtles--server)))

(defun turtles-start ()
  (interactive)
  (unless (turtles-io-server-live-p turtles--server)
    (server-ensure-safe-dir server-socket-dir)
    (setq turtles--server
          (turtles-io-server
           (expand-file-name (format "turtles-%s" (emacs-pid))
                             server-socket-dir)
           `((grab . ,(turtles-io-method-handler (_ignored)
                        (with-current-buffer (get-buffer turtles-buffer-name)
                          (buffer-substring term-home-marker (point-max)))))))))

  (let ((buf (get-buffer-create turtles-buffer-name)))
    (unless (and (turtles-io-conn-live-p turtles--conn)
                 (term-check-proc buf))
      (mapc (lambda (c) (turtles-io-call-method c 'exit nil nil))
            (turtles-io-server-connections turtles--server))
      (setq turtles--conn nil)
      (setf (turtles-io-server-on-new-connection turtles--server)
            (lambda (conn)
              (setf turtles--conn conn)
              (setf (turtles-io-server-on-new-connection turtles--server) nil)))
      (unwind-protect
          (progn
           (with-current-buffer buf (term-mode))
           (term-exec
            buf
            "*turtles*"
            (expand-file-name invocation-name invocation-directory)
            nil
            (append '("-nw" "-Q")
                    (turtles--dirs-from-load-path)
                    `("-l" ,turtles--file-name)))

           (with-current-buffer (get-buffer turtles-buffer-name) ;; buf
             (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
             (term-send-raw-string
              (format "\033xturtles--launch\n%s\n"
                      (turtles-io-server-socket turtles--server))))

           (turtles-io-wait-for 5 "Turtles Emacs failed to connect"
                                (lambda () turtles--conn)))
        (setf (turtles-io-server-on-new-connection turtles--server) nil)))))

(defun turtles-stop ()
  (interactive)
  (when (turtles-io-server-live-p turtles--server)
    (mapc (lambda (c) (turtles-io-call-method c 'exit nil nil))
          (turtles-io-server-connections turtles--server))
    (delete-process (turtles-io-server-proc turtles--server)))
  (setq turtles--server nil)
  (setq turtles--conn nil)

  (when-let ((buf (get-buffer turtles-buffer-name)))
    (kill-buffer buf)))

(defun turtles-fail-unless-live ()
  (unless (turtles-io-conn-live-p turtles--conn)
    (error "No Turtles! Call turtles-start")))

(defun turtles--dirs-from-load-path ()
  (let ((args nil))
    (dolist (path load-path)
      (push "-L" args)
      (push path args))
    (nreverse args)))

(defun turtles--launch (socket)
  (interactive "F")
  (setq turtles--conn
        (turtles-io-connect socket
                            `((eval . ,(turtles-io-method-handler (expr)
                                         (eval expr)))
                              (exit . ,(lambda (_conn _id _method _params)
                                         (kill-emacs nil)))))))

(defun turtles-grab-frame-into (buffer)
  "Grab a snapshot current frame into BUFFER.

This includes all windows and decorations. Unless that's what you
want to test, it's usually better to call `turtles-grab-buffer'
or `turtles-grab-win', which just return the window body."
  (turtles-fail-unless-live)
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))
    (insert (turtles-io-call-method-and-wait turtles--conn 'grab))
    (font-lock-mode)))

(provide 'turtles)
