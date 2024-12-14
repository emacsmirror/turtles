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

(defun turtles-test-init-buffer ()
  (setq-local truncate-lines t)
  (setq-local left-margin-width 0))

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

(ert-deftest turtles-test-grab-buffer-head ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (turtles-test-init-buffer)
    (dotimes (i 100)
      (insert (format "line %d\n" i)))
    (goto-char (point-min))
    (should (equal
             (concat "line 0\n"
                     "line 1\n"
                     "line 2\n"
                     "line 3\n"
                     "line 4\n"
                     "line 5\n"
                     "line 6\n"
                     "line 7\n"
                     "line 8\n"
                     "line 9\n"
                     "line 10\n"
                     "line 11\n"
                     "line 12\n"
                     "line 13\n"
                     "line 14\n"
                     "line 15\n"
                     "line 16\n"
                     "line 17\n")
             (turtles-grab-buffer-to-string (current-buffer))))))


(ert-deftest turtles-test-grab-buffer-tail ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (turtles-test-init-buffer)
    (dotimes (i 100)
      (insert (format "line %d\n" i)))
    (goto-char (point-max))
    (should (equal
             (concat "line 91\n"
                     "line 92\n"
                     "line 93\n"
                     "line 94\n"
                     "line 95\n"
                     "line 96\n"
                     "line 97\n"
                     "line 98\n"
                     "line 99\n"
                     "\n"
                     "\n"
                     "\n"
                     "\n"
                     "\n"
                     "\n"
                     "\n"
                     "\n"
                     "\n")
             (turtles-grab-buffer-to-string (current-buffer))))))

(ert-deftest turtles-test-grab-buffer-full-lines ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (turtles-test-init-buffer)
    (dotimes (i 100)
      (insert (format "line %d%s\n" i (make-string 80 ?-))))
    (goto-char (point-min))
    (should (equal
             (concat "line 0-------------------------------------------------------------------------$\n"
                     "line 1-------------------------------------------------------------------------$\n"
                     "line 2-------------------------------------------------------------------------$\n"
                     "line 3-------------------------------------------------------------------------$\n"
                     "line 4-------------------------------------------------------------------------$\n"
                     "line 5-------------------------------------------------------------------------$\n"
                     "line 6-------------------------------------------------------------------------$\n"
                     "line 7-------------------------------------------------------------------------$\n"
                     "line 8-------------------------------------------------------------------------$\n"
                     "line 9-------------------------------------------------------------------------$\n"
                     "line 10------------------------------------------------------------------------$\n"
                     "line 11------------------------------------------------------------------------$\n"
                     "line 12------------------------------------------------------------------------$\n"
                     "line 13------------------------------------------------------------------------$\n"
                     "line 14------------------------------------------------------------------------$\n"
                     "line 15------------------------------------------------------------------------$\n"
                     "line 16------------------------------------------------------------------------$\n"
                     "line 17------------------------------------------------------------------------$\n")
             (turtles-grab-buffer-to-string (current-buffer))))))


(ert-deftest turtles-test-grab-window-horiz-center ()
  (turtles-ert-test)

  (let (buf1 buf2 center-win)
    (ert-with-test-buffer (:name "buf1")
      (turtles-test-init-buffer)
      (setq buf1 (current-buffer))
      (dotimes (i 40)
        (insert (format "line %d%s\n" i (make-string 80 ?-))))
      (goto-char (point-min))

      (ert-with-test-buffer (:name "buf2")
        (turtles-test-init-buffer)
        (setq buf2 (current-buffer))
        (dotimes (_ 40)
          (insert (make-string 80 ?x)))
        (goto-char (point-min))

        (set-window-buffer (frame-root-window) buf2)
        (setq center-win (split-window-below 5 (frame-root-window)))
        (split-window-below 10 center-win)
        (set-window-buffer center-win buf1)

        (should
         (equal
          (concat
           "line 0-------------------------------------------------------------------------$\n"
           "line 1-------------------------------------------------------------------------$\n"
           "line 2-------------------------------------------------------------------------$\n"
           "line 3-------------------------------------------------------------------------$\n"
           "line 4-------------------------------------------------------------------------$\n"
           "line 5-------------------------------------------------------------------------$\n"
           "line 6-------------------------------------------------------------------------$\n"
           "line 7-------------------------------------------------------------------------$\n"
           "line 8-------------------------------------------------------------------------$\n")
          (turtles-grab-window-to-string center-win)))))))


(ert-deftest turtles-test-grab-window-vert-center ()
  (turtles-ert-test)

  (let (buf1 buf2 center-win)
    (ert-with-test-buffer (:name "buf1")
      (turtles-test-init-buffer)
      (setq buf1 (current-buffer))
      (dotimes (i 40)
        (insert (format "line %d%s\n" i (make-string 80 ?-))))
      (goto-char (point-min))

      (ert-with-test-buffer (:name "buf2")
        (turtles-test-init-buffer)

        (setq buf2 (current-buffer))
        (insert "begin")
        (dotimes (_ 40)
          (insert "\n"))
        (insert "end")
        (goto-char (point-min))

        (set-window-buffer (frame-root-window) buf2)
        (setq center-win (split-window-right 20 (frame-root-window)))
        (split-window-right 20 center-win)
        (set-window-buffer center-win buf1)

        (should
         (equal
          (concat
           "line 0------------$\n"
           "line 1------------$\n"
           "line 2------------$\n"
           "line 3------------$\n"
           "line 4------------$\n"
           "line 5------------$\n"
           "line 6------------$\n"
           "line 7------------$\n"
           "line 8------------$\n"
           "line 9------------$\n"
           "line 10-----------$\n"
           "line 11-----------$\n"
           "line 12-----------$\n"
           "line 13-----------$\n"
           "line 14-----------$\n"
           "line 15-----------$\n"
           "line 16-----------$\n"
           "line 17-----------$\n")
          (turtles-grab-window-to-string center-win)))))))


(ert-deftest turtles-test-grab-window-vert-center-empty-buffer ()
  ;; Terminal display is broken when run in batch mode: the lines of a
  ;; window to the right of a buffer showing a line with no final \n
  ;; on it are truncated, so the positions don't align.
  ;;
  ;; So the terminal would look like this:
  ;;
  ;; mostly empty       |line 0------------$|mostly empty
  ;;                    |line 1------------$|
  ;; . |line 2------------$|.
  ;;  |line 3------------$|
  ;;  |line 4------------$|
  ;;  |line 5------------$|
  ;;  |line 6------------$|
  ;;  |line 7------------$|
  ;; [...]
  ;;
  ;; This is puzzling and very annoying as tests are primarily run in
  ;; batch modes.
  (turtles-ert-test)

  (let (buf1 buf2 center-win)
    (ert-with-test-buffer (:name "buf1")
      (turtles-test-init-buffer)
      (setq buf1 (current-buffer))
      (dotimes (i 40)
        (insert (format "line %d%s\n" i (make-string 80 ?-))))
      (goto-char (point-min))

      (ert-with-test-buffer (:name "buf2")
        (turtles-test-init-buffer)

        (setq buf2 (current-buffer))
        (insert "mostly empty\n\n.")
        (goto-char (point-min))

        (set-window-buffer (frame-root-window) buf2)
        (setq center-win (split-window-right 20 (frame-root-window)))
        (split-window-right 20 center-win)
        (set-window-buffer center-win buf1)

        (should
         (equal
          (concat
           "line 0------------$\n"
           "line 1------------$\n"
           "line 2------------$\n"
           "line 3------------$\n"
           "line 4------------$\n"
           "line 5------------$\n"
           "line 6------------$\n"
           "line 7------------$\n"
           "line 8------------$\n"
           "line 9------------$\n"
           "line 10-----------$\n"
           "line 11-----------$\n"
           "line 12-----------$\n"
           "line 13-----------$\n"
           "line 14-----------$\n"
           "line 15-----------$\n"
           "line 16-----------$\n"
           "line 17-----------$\n")
          (turtles-grab-window-to-string center-win)))))))

(ert-deftest turtles-test-grab-point ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (dotimes (i 10)
        (insert (format "line %d." i))
        ;; Make sure position computation isn't confused by invisible
        ;; text.
        (insert (propertize "invisible text" 'invisible t))
        (insert ".\n"))
      (goto-char (point-min))
      (search-forward "line 6")
      (should
       (equal
        (concat "line 0..\n"
                "line 1..\n"
                "line 2..\n"
                "line 3..\n"
                "line 4..\n"
                "line 5..\n"
                "line 6<>..\n"
                "line 7..\n"
                "line 8..\n"
                "line 9..")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer-into test-buffer (current-buffer))
           (insert "<>")
           (buffer-string))))))))


(ert-deftest turtles-test-grab-active-mark ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (dotimes (i 10)
        (insert (format "line %d." i))
        ;; Make sure position computation isn't confused by invisible
        ;; text.
        (insert (propertize "invisible text" 'invisible t))
        (insert ".\n"))

      (goto-char (point-min))
      (search-forward "line 3")
      (push-mark (match-beginning 0) 'nomsg)
      (search-forward "line 6")
      (activate-mark)
      (should (and t (region-active-p)))

      (should
       (equal
        (concat "line 0..\n"
                "line 1..\n"
                "line 2..\n"
                ;; The active regions has spaces after the text
                ;; because the region is highlighted on the screen.
                "[line 3..                                                                        \n"
                "line 4..                                                                        \n"
                "line 5..                                                                        \n"
                "line 6]..\n"
                "line 7..\n"
                "line 8..\n"
                "line 9..")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer-into test-buffer (current-buffer))
           (insert "]")
           (goto-char (mark))
           (insert "[")
           (should (region-active-p))

           (buffer-string))))))))


(ert-deftest turtles-test-grab-inactive-mark ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (dotimes (i 10)
        (insert (format "line %d." i))
        ;; Make sure position computation isn't confused by invisible
        ;; text.
        (insert (propertize "invisible text" 'invisible t))
        (insert ".\n"))

      (goto-char (point-min))
      (search-forward "line 3")
      (push-mark (match-beginning 0) 'nomsg)
      (search-forward "line 6")
      (deactivate-mark)
      (should (not (region-active-p)))

      (should
       (equal
        (concat "line 0..\n"
                "line 1..\n"
                "line 2..\n"
                "[line 3..\n"
                "line 4..\n"
                "line 5..\n"
                "line 6]..\n"
                "line 7..\n"
                "line 8..\n"
                "line 9..")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer-into test-buffer (current-buffer))
           (insert "]")
           (goto-char (mark))
           (insert "[")
           (should-not (region-active-p))

           (buffer-string))))))))

(ert-deftest turtles-test-grab-mark-before-window-start ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (dotimes (i 100)
        (insert (format "line %d.\n" i)))

      (goto-char (point-min))
      (search-forward "line 3")
      (push-mark (match-beginning 0) 'nomsg)
      (search-forward "line 90")

      (should
       (equal
        (concat "[line 81.\n"
                "line 82.\n"
                "line 83.\n"
                "line 84.\n"
                "line 85.\n"
                "line 86.\n"
                "line 87.\n"
                "line 88.\n"
                "line 89.\n"
                "line 90].\n"
                "line 91.\n"
                "line 92.\n"
                "line 93.\n"
                "line 94.\n"
                "line 95.\n"
                "line 96.\n"
                "line 97.\n"
                "line 98.")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer-into test-buffer (current-buffer))
           (insert "]")
           (goto-char (mark))
           (insert "[")

           (buffer-string))))))))

(ert-deftest turtles-test-grab-mark-after-window-start ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (dotimes (i 100)
        (insert (format "line %d.\n" i)))

      (goto-char (point-min))
      (search-forward "line 90")
      (push-mark (point) 'nomsg)
      (goto-char (point-min))
      (search-forward "line 9")
      (goto-char (match-beginning 0))

      (should
       (equal
        (concat
         "line 0.\n"
         "line 1.\n"
         "line 2.\n"
         "line 3.\n"
         "line 4.\n"
         "line 5.\n"
         "line 6.\n"
         "line 7.\n"
         "line 8.\n"
         "[line 9.\n"
         "line 10.\n"
         "line 11.\n"
         "line 12.\n"
         "line 13.\n"
         "line 14.\n"
         "line 15.\n"
         "line 16.\n"
         "line 17.\n"
         "]")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer-into test-buffer (current-buffer))
           (insert "[")
           (goto-char (mark))
           (insert "]")

           (buffer-string))))))))


(ert-deftest turtles-test-grab-invisible-mark ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (dotimes (i 10)
        (insert (format "line %d." i))
        ;; Make sure position computation isn't confused by invisible
        ;; text.
        (insert (propertize "invisible text" 'invisible t))
        (insert ".\n"))

      ;; Put the mark in the middle of an invisible section.
      (goto-char (point-min))
      (search-forward "line 3")
      (search-forward "invisible")
      (push-mark (point) 'nomsg)
      (search-forward "line 6")

      (should
       (equal
        (concat "line 0..\n"
                "line 1..\n"
                "line 2..\n"
                "line 3.[.\n"
                "line 4..\n"
                "line 5..\n"
                "line 6]..\n"
                "line 7..\n"
                "line 8..\n"
                "line 9..")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer-into test-buffer (current-buffer))
           (insert "]")
           (goto-char (mark))
           (insert "[")

           (buffer-string))))))))

(ert-deftest turtles-test-grab-buffer-position ()
  (turtles-ert-test)

  (let (test-buffer pos)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (dotimes (i 10)
        (insert (format "line %d." i))
        ;; Make sure position computation isn't confused by invisible
        ;; text.
        (insert (propertize "invisible text" 'invisible t))
        (insert ".\n"))
      (goto-char (point-min))
      (search-forward "line 6")
      (setq pos (1- (match-end 0)))
      (goto-char (point-min))
      (should
       (equal
        (concat "line 0..\n"
                "line 1..\n"
                "line 2..\n"
                "line 3..\n"
                "line 4..\n"
                "line 5..\n"
                "line <>6..\n"
                "line 7..\n"
                "line 8..\n"
                "line 9..")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer-into test-buffer (current-buffer))
           (goto-char (turtles-pos-in-window-grab pos))
           (insert "<>")
           (buffer-string))))))))

(ert-deftest turtles-test-mark-region ()
  (ert-with-test-buffer ()
   (insert "Time is a drug. Too much of it kills you.")

   (goto-char (point-min))
   (search-forward "Too")
   (push-mark (match-beginning 0))
   (search-forward "kills")
   (activate-mark)

   (turtles-mark-region "[]")

   (should (equal "Time is a drug. [Too much of it kills] you."
                  (buffer-string)))))

(ert-deftest turtles-test-mark-region-swapped ()
  (ert-with-test-buffer ()
   (insert "Time is a drug. Too much of it kills you.")

   (goto-char (point-min))
   (search-forward "kills")
   (push-mark (point) 'nomsg)
   (goto-char (point-min))
   (search-forward "Too")
   (goto-char (match-beginning 0))
   (activate-mark)

   (turtles-mark-region "[]")

   (should (equal "Time is a drug. [Too much of it kills] you."
                  (buffer-string)))))

(ert-deftest turtles-test-mark-region-twochars ()
  (ert-with-test-buffer ()
   (insert "Time is a drug. Too much of it kills you.")

   (goto-char (point-min))
   (search-forward "kills")
   (push-mark (point) 'nomsg)
   (goto-char (point-min))
   (search-forward "Too")
   (goto-char (match-beginning 0))
   (activate-mark)

   (turtles-mark-region "/**/")

   (should (equal "Time is a drug. /*Too much of it kills*/ you."
                  (buffer-string)))))

(ert-deftest turtles-test-mark-region-opening-and-closing ()
  (ert-with-test-buffer ()
   (insert "Time is a drug. Too much of it kills you.")

   (goto-char (point-min))
   (search-forward "kills")
   (push-mark (point) 'nomsg)
   (goto-char (point-min))
   (search-forward "Too")
   (goto-char (match-beginning 0))
   (activate-mark)

   (turtles-mark-region ">>" "<")

   (should (equal "Time is a drug. >>Too much of it kills< you."
                  (buffer-string)))))

(ert-deftest turtles-test-colors ()
  (turtles-ert-test)

  (let (orig-buf capture-buf)
    (ert-with-test-buffer (:name "orig")
      (setq orig-buf (current-buffer))
      (turtles-test-init-buffer)
      (insert (propertize "green on red" 'face
                          `(:foreground ,(face-foreground 'ansi-color-green)
                                        :background ,(face-background 'ansi-color-red))))
      (insert "\n")
      (insert (propertize "yellow on blue" 'face
                          `(:foreground ,(face-foreground 'ansi-color-yellow)
                                        :background ,(face-background 'ansi-color-blue))))
      (insert "\n")
      (ert-with-test-buffer (:name "capture")
        (setq capture-buf (current-buffer))
        (turtles-grab-buffer-into orig-buf capture-buf)
        (goto-char (point-min))

        (should (search-forward "green on red"))
        (goto-char (match-beginning 0))
        (should (equal (color-values (face-foreground 'ansi-color-green))
                       (color-values (foreground-color-at-point))))
        (should (equal (color-values (face-background 'ansi-color-red))
                       (color-values (background-color-at-point))))

        (should (search-forward "yellow on blue"))
        (goto-char (match-beginning 0))
        (should (equal (color-values (face-foreground 'ansi-color-yellow))
                       (color-values (foreground-color-at-point))))
        (should (equal (color-values (face-background 'ansi-color-blue))
                       (color-values (background-color-at-point))))))))
