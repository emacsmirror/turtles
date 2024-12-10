;; termgrab-ert-test.el --- Test termgrab-ert.el -*- lexical-binding: t -*-

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
(require 'termgrab-ert)

(ert-deftest termgrab-ert-test-to-string-noarg ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (insert "hello, world")
    (should (equal "hello, world" (termgrab-to-string)))))

(ert-deftest termgrab-ert-test-to-string-buf ()
  (termgrab-start-server)
  (let (test-buffer)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (insert "hello, world")
      (with-temp-buffer
        (should (equal "hello, world" (termgrab-to-string :buf test-buffer)))))))

(ert-deftest termgrab-ert-test-to-string-win ()
  (termgrab-start-server)
  (let (buf1 buf2)
    (ert-with-test-buffer (:name "buf1")
      (setq buf1 (current-buffer))
      (insert "hello, world")

      (ert-with-test-buffer (:name "buf2")
        (setq buf2 (current-buffer))
        (insert "foobar")

        (split-window-below nil (termgrab-root-window))
        (pcase-let ((`(,win1 ,win2) (window-list termgrab-frame)))
          (set-window-buffer win1 buf1)
          (set-window-buffer win2 buf2)
                       
      (should (equal "hello, world" (termgrab-to-string :win win1)))
      (should (equal "foobar" (termgrab-to-string :win win2))))))))

(ert-deftest termgrab-ert-test-to-string-faces ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (insert (propertize "hello" 'face 'error))
    (insert ", ")
    (insert (propertize "world" 'face 'success))
    (should (equal "[hello], {world}"
                   (termgrab-to-string :faces '((error "[]")
                                                (success "{" "}")))))))

(ert-deftest termgrab-ert-test-to-string-region ()
  (termgrab-start-server)
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
      (termgrab-to-string :region "[]")))))

(ert-deftest termgrab-ert-test-to-string-point ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (insert "baa, baa, black sheep, have you any wool?")
    (goto-char (point-min))
    (search-forward "black")

    (should
     (equal
      "baa, baa, black>< sheep, have you any wool?"
      (termgrab-to-string :point "><")))))

(ert-deftest termgrab-ert-test-to-string-point-and-region ()
  (termgrab-start-server)
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
      (termgrab-to-string :point "><" :region "[]")))))

(ert-deftest termgrab-ert-test-with-grab-buffer-noarg ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (insert "hello, world")
    (termgrab-with-grab-buffer ()
      (should (equal "hello, world"
                     (string-trim (buffer-string)))))))

(ert-deftest termgrab-ert-test-with-grab-buffer-result ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (insert "hello, world")
    (should (equal "hello, world"
                   (termgrab-with-grab-buffer ()
                     (string-trim (buffer-string)))))))

(ert-deftest termgrab-ert-test-with-grab-buffer-buf ()
  (termgrab-start-server)
  (let (test-buffer)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (insert "hello, world")
      (with-temp-buffer
        (termgrab-with-grab-buffer (:buf test-buffer)
          (should (equal "hello, world"
                         (string-trim (buffer-string)))))))))


(ert-deftest termgrab-ert-test-with-grab-buffer-win ()
  (termgrab-start-server)
  (let (buf1 buf2)
    (ert-with-test-buffer (:name "buf1")
      (setq buf1 (current-buffer))
      (insert "hello, world")

      (ert-with-test-buffer (:name "buf2")
        (setq buf2 (current-buffer))
        (insert "foobar")

        (split-window-below nil (termgrab-root-window))
        (pcase-let ((`(,win1 ,win2) (window-list termgrab-frame)))
          (set-window-buffer win1 buf1)
          (set-window-buffer win2 buf2)

          (termgrab-with-grab-buffer (:win win1)
            (should (equal "hello, world"
                           (string-trim (buffer-string)))))
          (termgrab-with-grab-buffer (:win win2)
            (should (equal "foobar"
                           (string-trim (buffer-string))))))))))

(ert-deftest termgrab-ert-test-with-grab-buffer-faces ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (insert (propertize "error" 'face 'error))
    (insert ", ")
    (insert (propertize "success" 'face 'success))
    (termgrab-with-grab-buffer (:faces '(success error))
      (goto-char (point-min))
      (search-forward "error")
      (should (equal 'error (get-text-property (1- (point)) 'face)))
      (search-forward "success")
      (should (equal 'success (get-text-property (1- (point)) 'face))))))
