;; turtles-ert-test.el --- Test turtles-ert.el -*- lexical-binding: t -*-

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

(require 'turtles-ert)

(ert-deftest turtles-ert-test-pass ()
  (turtles-ert-test)

  (should (equal 1 1)))

(ert-deftest turtles-ert-test-fail ()
  :expected-result :failed
  (turtles-ert-test)

  (should (equal 1 2)))

(ert-deftest turtles-ert-test-to-string-noarg ()
  (turtles-ert-test)
  (ert-with-test-buffer ()
    (insert "hello, world")
    (should (equal "hello, world" (turtles-to-string)))))

(ert-deftest turtles-ert-test-to-string-buf ()
  (turtles-ert-test)

  (let (test-buffer)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (insert "hello, world")
      (with-temp-buffer
        (should (equal "hello, world" (turtles-to-string :buf test-buffer)))))))

(ert-deftest turtles-ert-test-to-string-win ()
  (turtles-ert-test)

  (let (buf1 buf2)
    (ert-with-test-buffer (:name "buf1")
      (setq buf1 (current-buffer))
      (insert "hello, world")

      (ert-with-test-buffer (:name "buf2")
        (setq buf2 (current-buffer))
        (insert "foobar")

        (split-window-below nil (frame-root-window))
        (pcase-let ((`(,win1 ,win2) (window-list)))
          (set-window-buffer win1 buf1)
          (set-window-buffer win2 buf2)

      (should (equal "hello, world" (turtles-to-string :win win1)))
      (should (equal "foobar" (turtles-to-string :win win2))))))))

(ert-deftest turtles-ert-test-to-string-faces ()
  (turtles-ert-test)
  (ert-with-test-buffer ()
    (insert (propertize "hello" 'face 'error))
    (insert ", ")
    (insert (propertize "world" 'face 'success))
    (should (equal "[hello], {world}"
                   (turtles-to-string :faces '((error "[]")
                                                (success "{" "}")))))))

(ert-deftest turtles-ert-test-to-string-region ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert "baa, baa, black sheep, have you any wool?")
    (goto-char (point-min))
    (search-forward "baa, black")
    (push-mark (match-beginning 0) 'nomsg)
    (search-forward "sheep")
    (activate-mark)

    (should
     (equal
      "baa, [baa, black sheep], have you any wool?"
      (turtles-to-string :region "[]")))))

(ert-deftest turtles-ert-test-to-string-point ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert "baa, baa, black sheep, have you any wool?")
    (goto-char (point-min))
    (search-forward "black")

    (should
     (equal
      "baa, baa, black>< sheep, have you any wool?"
      (turtles-to-string :point "><")))))

(ert-deftest turtles-ert-test-to-string-point-and-region ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert "baa, baa, black sheep, have you any wool?")
    (goto-char (point-min))
    (search-forward "baa, black")
    (push-mark (match-beginning 0) 'nomsg)
    (search-forward "sheep")
    (activate-mark)

    (should
     (equal
      "baa, [baa, black sheep><], have you any wool?"
      (turtles-to-string :point "><" :region "[]")))))

(ert-deftest turtles-ert-test-with-grab-buffer-noarg ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert "hello, world")
    (turtles-with-grab-buffer ()
      (should (equal "hello, world"
                     (string-trim (buffer-string)))))))

(ert-deftest turtles-ert-test-with-grab-buffer-result ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert "hello, world")
    (should (equal "hello, world"
                   (turtles-with-grab-buffer ()
                     (string-trim (buffer-string)))))))

(ert-deftest turtles-ert-test-with-grab-buffer-buf ()
  (turtles-ert-test)

  (let (test-buffer)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (insert "hello, world")
      (with-temp-buffer
        (turtles-with-grab-buffer (:buf test-buffer)
          (should (equal "hello, world"
                         (string-trim (buffer-string)))))))))


(ert-deftest turtles-ert-test-with-grab-buffer-win ()
  (turtles-ert-test)

  (let (buf1 buf2)
    (ert-with-test-buffer (:name "buf1")
      (setq buf1 (current-buffer))
      (insert "hello, world")

      (ert-with-test-buffer (:name "buf2")
        (setq buf2 (current-buffer))
        (insert "foobar")

        (split-window-below nil (frame-root-window))
        (pcase-let ((`(,win1 ,win2) (window-list)))
          (set-window-buffer win1 buf1)
          (set-window-buffer win2 buf2)

          (turtles-with-grab-buffer (:win win1)
            (should (equal "hello, world"
                           (string-trim (buffer-string)))))
          (turtles-with-grab-buffer (:win win2)
            (should (equal "foobar"
                           (string-trim (buffer-string))))))))))

(ert-deftest turtles-ert-test-with-grab-buffer-faces ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert (propertize "error" 'face 'error))
    (insert ", ")
    (insert (propertize "success" 'face 'success))
    (turtles-with-grab-buffer (:faces '(success error))
      (goto-char (point-min))
      (search-forward "error")
      (should (equal 'error (get-text-property (1- (point)) 'face)))
      (search-forward "success")
      (should (equal 'success (get-text-property (1- (point)) 'face))))))

(ert-deftest turtles-ert-test-with-grab-buffer-faces-and-marks ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert (propertize "error" 'face 'error))
    (insert ", ")
    (insert (propertize "success" 'face 'success))
    (turtles-with-grab-buffer (:faces '(success (error . "{}") ))
      (turtles-trim-buffer)
      (should (equal "{error}, success" (buffer-string)))
      (goto-char (point-min))
      (search-forward "success")
      (should (equal 'success (get-text-property (1- (point)) 'face))))))

(ert-deftest turtles-ert-read-from-minibuffer ()
  (turtles-ert-test)

  (ert-with-buffer-selected ()
    (should
     (equal "hello"
            (turtles-read-from-minibuffer
                (read-from-minibuffer "Prompt: ")
              (execute-kbd-macro (kbd "hello"))
              (should (equal "Prompt: hello" (turtles-to-string)))
              (execute-kbd-macro (kbd "RET")))))))
