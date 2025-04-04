;;; turtles-instance-test.el --- Test turtles-instance.el -*- lexical-binding: t -*-

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
(require 'ert)
(require 'ert-x)
(require 'term)
(require 'turtles)
(require 'turtles-instance)

(defvar term-width) ;; term.el
(defvar term-height) ;; term.el

(turtles-definstance turtles--restart ()
  "A private test instance to test restart.")

(defvar turtles-test-var1 'default)
(defvar turtles-test-var2 'default)
(defvar turtles-test-var3 'default)
(turtles-definstance turtles--forward (:forward '(turtles-test-var1 turtles-test-var2 turtles-test-var3))
  "A private test instance to test variable value forwarding.")

(ert-deftest turtles-instance-restart ()
  (turtles-start-server)
  (should turtles--server)
  (should (turtles-io-server-live-p turtles--server))

  (let ((inst (turtles-get-instance 'turtles--restart))
        buf proc)
    (should inst)
    (should (eq inst (turtles-stop-instance inst)))
    (should (eq inst (turtles-start-instance inst)))
    (should (turtles-instance-live-p inst))

    (setq buf (turtles-instance-term-buf inst))
    (should (buffer-live-p buf))
    (should (process-live-p (get-buffer-process buf)))

    (setq proc (turtles-io-conn-proc (turtles-instance-conn inst)))
    (should (process-live-p proc))

    (should (equal "ok" (turtles-instance-eval inst "ok")))

    (turtles-stop-instance inst)

    (should-not (turtles-instance-live-p inst))
    (should-not (buffer-live-p buf))
    (should-not (process-live-p proc))))

(ert-deftest turtles-instance-forward ()
  ;; Only on the server: set values different from the default which
  ;; will have to be forwarded.

  (turtles-stop-instance 'turtles--forward)
  (setq turtles-test-var1 505)
  (setq turtles-test-var2 "barfoo")
  (setq turtles-test-var3 '("hello" "world"))

  (let ((inst (turtles-start-instance 'turtles--forward)))
    (should
     (equal
      (list 505 "barfoo" '("hello" "world"))
      (turtles-instance-eval
       inst '(list turtles-test-var1 turtles-test-var2 turtles-test-var3))))))

(ert-deftest turtles-instance-message ()
  (let ((inst (turtles-start-instance 'default)))
    (let ((inhibit-message t))
      (ert-with-message-capture messages
        (turtles-instance-eval inst
         '(let ((turtles-send-messages-upstream t))
            (message "hello from turtles-test-message")))
        (should (equal "[default] hello from turtles-test-message\n" messages))))))

(ert-deftest turtles-instance-inhibit-message ()
  (let ((inst (turtles-start-instance 'default)))
    (ert-with-message-capture messages
      (turtles-instance-eval inst
         '(let ((turtles-send-messages-upstream t)
                (inhibit-message t))
            (message "hello from turtles-test-message")))
      (should (equal "" messages)))))

(ert-deftest turtles-instance-default-size ()
  (let ((inst (turtles-start-instance 'default)))
    (with-current-buffer (turtles-instance-term-buf inst)
      (should (equal 80 term-width))
      (should (equal 24 term-height)))

    (should (equal
             (cons 80 24)
             (turtles-instance-eval
              inst '(cons (frame-width) (frame-height)))))))

(ert-deftest turtles-instance-turtles-larger-size ()
  (let ((inst (turtles-start-instance 'larger)))
    (with-current-buffer (turtles-instance-term-buf inst)
      (should (equal 132 term-width))
      (should (equal 43 term-height)))
    (should (equal
             (cons 132 43)
             (turtles-instance-eval
              inst '(cons (frame-width) (frame-height)))))))

(ert-deftest turtles-instance-unreadable-buffer ()
  (let ((inst (turtles-start-instance 'default)))
    (should
     (equal '(turtles-buffer :name "*scratch*" :instance default)
            (turtles-instance-eval inst
             '(get-scratch-buffer-create))))))

(ert-deftest turtles-instance-customize-send-messages-upstream ()
  (let ((inst (turtles-start-instance 'default))
        (original-value turtles-send-messages-upstream)
        (inhibit-message t)
        conn)
    (setq conn (turtles-instance-conn inst))

    (unwind-protect
        (ert-with-message-capture messages
          (custom-set-variables '(turtles-send-messages-upstream t now))
          (should turtles-send-messages-upstream)
          (should (turtles-instance-eval inst 'turtles-send-messages-upstream))
          (turtles-instance-eval inst '(message "message1, sent upstream"))

          (custom-set-variables '(turtles-send-messages-upstream nil now))
          (should-not turtles-send-messages-upstream)
          (should-not (turtles-io-call-method conn 'eval 'turtles-send-messages-upstream))
          (turtles-instance-eval inst '(message "message2, not sent upstream"))

          (should (member "[default] message1, sent upstream"
                          (string-split messages "\n")))
          (should-not (member "[default] message2, not sent upstream"
                              (string-split messages "\n")))

          (custom-set-variables `(turtles-send-messages-upstream ,original-value now)))
      (setq turtles-send-messages-upstream original-value))))

(ert-deftest turtles-term-substring-with-properties ()
  (ert-with-test-buffer ()
    (let ((inhibit-read-only t))
      (insert (propertize "This buffer " 'read-only t))
      (insert (propertize "is" 'font-lock-face '(:foreground "purple")))
      (insert " ")
      (insert (propertize "in" 'face '(:foreground "yellow")))
      (insert " ")
      (insert (propertize "full"
                          'font-lock-face '(:foreground "red")
                          'cursor-face '(:background "cyan")))
      (insert " ")
      (insert (propertize "color" 'face '(:foreground "blue"))))

    (let ((str (turtles--substring-with-properties
                ;; start
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "buffer")
                  (match-beginning 0))
                ;; end
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "full"))
                '((face . face) (font-lock-face . face)))))

      (should (equal "buffer is in full" str))
      (should (equal '(:foreground "purple") (get-text-property 7 'face str)))
      (should (equal '(:foreground "purple") (get-text-property 8 'face str)))
      (should (equal nil (get-text-property 9 'face str)))
      (should (equal '(:foreground "yellow") (get-text-property 10 'face str)))
      (should (equal '(:foreground "yellow") (get-text-property 11 'face str)))
      (should (equal nil (get-text-property 12 'face str)))
      (should (equal '(:foreground "red") (get-text-property 13 'face str)))
      (should (equal '(:foreground "red") (get-text-property 16 'face str))))))

(turtles-ert-deftest turtles-restart ()
  (turtles-shutdown)
  (should-not (turtles-live-instances))

  (unwind-protect
      (let ((restart-inst (turtles-get-instance 'turtles--restart))
            (default-inst (turtles-get-instance 'default))
            (inhibit-message t))
        (should-not (turtles-instance-live-p restart-inst))
        (should-not (turtles-instance-live-p default-inst))

        (turtles-start-instance restart-inst)
        (should (turtles-instance-live-p restart-inst))
        (should-not (turtles-instance-live-p default-inst))

        (turtles-instance-eval restart-inst '(progn
                                               (defvar turtles--instance-test-id nil)
                                               (setq turtles--instance-test-id 1)))
        (should (equal 1 (turtles-instance-eval restart-inst 'turtles--instance-test-id)))

        (turtles-restart)
        (should (turtles-instance-live-p restart-inst))
        (should-not (turtles-instance-live-p default-inst))
        (should-not (turtles-instance-eval restart-inst '(progn
                                                           (defvar turtles--instance-test-id nil)
                                                           turtles--instance-test-id))))
    (turtles-shutdown)))

(turtles-ert-deftest turtles-read-instance ()
  ;; We want only two instances for this test.
  (let* ((default-inst (turtles-get-instance 'default))
         (restart-inst (turtles-get-instance 'turtles--restart))
         (turtles-instance-alist `((default . ,default-inst)
                                   (turtles--restart . ,restart-inst)))
         (inhibit-message t))
    (turtles-with-minibuffer
        (should (eq restart-inst (turtles-read-instance)))
     (turtles-with-grab-buffer (:name "initial")
       (should (equal "Instance:" (buffer-string))))

     ;; Make sure that the short documentation are provided correctly as annotations.
     (minibuffer-complete)
     (turtles-with-grab-buffer (:buf "*Completions*")
       (goto-char (point-max))
       (forward-line -1)
       (should (equal (concat "default Emacs instance to run tests on.\n"
                              "turtles--restart A private test instance to test restart.")
                      (buffer-substring (point) (point-max)))))

     ;; Select restart
     (execute-kbd-macro "turtles--restart"))))


(require 'turtles-instance)
