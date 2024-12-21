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

(require 'button)
(require 'compat)
(require 'ert)
(require 'ert-x)

(require 'turtles)
(require 'turtles-io)

(defun turtles-test-init-buffer ()
  (setq-local truncate-lines t)
  (setq-local left-margin-width 0))

(ert-deftest turtles-grab-frame ()
  (turtles-ert-test)

  (with-current-buffer (get-scratch-buffer-create)
    (turtles-display-buffer-full-frame (current-buffer))
    (insert "De Chelonian Mobile")
    (with-temp-buffer
      (turtles-grab-frame (current-buffer))
      (goto-char (point-min))
      (should (search-forward "De Chelonian Mobile")))))

(ert-deftest turtles-grab-buffer-head ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (turtles-test-init-buffer)
    (dotimes (i 100)
      (insert (format "line %d\n" i)))
    (goto-char (point-min))
    (turtles-with-grab-buffer ()
      (turtles-trim-buffer)
      (should
       (equal
        (concat
         "line 0\n"
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
         "line 17")
        (buffer-string))))))

(ert-deftest turtles-grab-buffer-tail ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (turtles-test-init-buffer)
    (dotimes (i 100)
      (insert (format "line %d\n" i)))
    (goto-char (point-max))
    (turtles-with-grab-buffer ()
      (turtles-trim-buffer)
      (should
       (equal
        (concat
         "line 91\n"
         "line 92\n"
         "line 93\n"
         "line 94\n"
         "line 95\n"
         "line 96\n"
         "line 97\n"
         "line 98\n"
         "line 99")
        (buffer-string))))))

(ert-deftest turtles-grab-buffer-full-lines ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (turtles-test-init-buffer)
    (dotimes (i 100)
      (insert (format "line %d%s\n" i (make-string 80 ?-))))
    (goto-char (point-min))
    (turtles-with-grab-buffer ()
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
        (buffer-string))))))

(ert-deftest turtles-grab-window-horiz-center ()
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

        (turtles-display-buffer-full-frame buf2)
        (select-window (frame-root-window))
        (setq center-win (split-window-below 5))
        (select-window center-win)
        (split-window-below 10)
        (set-window-buffer center-win buf1)
        (when (/= 10 (window-height center-win))
          (window-resize center-win (- 10 (window-height center-win))))

        (ert-with-test-buffer (:name "grab")
          (turtles-grab-window center-win (current-buffer))
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
            (buffer-string))))))))

(ert-deftest turtles-grab-window-vert-center ()
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

        (turtles-display-buffer-full-frame buf2)
        (select-window (frame-root-window))
        (setq center-win (split-window-right 20))
        (select-window center-win)
        (split-window-right 20)
        (set-window-buffer center-win buf1)

        (ert-with-test-buffer (:name "grab")
          (turtles-grab-window center-win (current-buffer))
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
            (buffer-string))))))))

(ert-deftest turtles-grab-window-vert-center-empty-buffer ()
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

        (turtles-display-buffer-full-frame buf2)
        (select-window (frame-root-window))
        (setq center-win (split-window-right 20))
        (select-window center-win)
        (split-window-right 20)
        (set-window-buffer center-win buf1)

        (ert-with-test-buffer (:name "grab")
          (turtles-grab-window center-win (current-buffer))
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
            (buffer-string))))))))

(ert-deftest turtles-grab-point ()
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
           (turtles-grab-buffer test-buffer)
           (turtles-mark-point "<>")
           (buffer-string))))))))

(ert-deftest turtles-grab-point-bottom-windows ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)

      ;; Above, show the scratch buffer, below, show this buffer. This
      ;; test makes sure that the point location computation is not
      ;; confused when window coordinate != display coordinate.
      (turtles-display-buffer-full-frame (get-scratch-buffer-create))
      (let ((win (split-window-below)))
        (set-window-buffer win (current-buffer))
        (select-window win))

      (dotimes (i 10)
        (insert (format "line %d..\n" i)))

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
                "line 7..")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer test-buffer)
           (turtles-mark-point "<>")
           (buffer-string))))))))

(ert-deftest turtles-grab-active-mark ()
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
                "[line 3..\n"
                "line 4..\n"
                "line 5..\n"
                "line 6]..\n"
                "line 7..\n"
                "line 8..\n"
                "line 9..")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer test-buffer)
           (insert "]")
           (goto-char (mark))
           (insert "[")
           (should (region-active-p))
           (delete-trailing-whitespace)

           (buffer-string))))))))

(ert-deftest turtles-grab-inactive-mark ()
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
           (turtles-grab-buffer test-buffer)
           (insert "]")
           (goto-char (mark))
           (insert "[")
           (should-not (region-active-p))

           (buffer-string))))))))

(ert-deftest turtles-grab-mark-before-window-start ()
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
           (turtles-grab-buffer test-buffer)
           (insert "]")
           (goto-char (mark))
           (insert "[")

           (buffer-string))))))))

(ert-deftest turtles-grab-mark-after-window-start ()
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
           (turtles-grab-buffer test-buffer)
           (insert "[")
           (goto-char (mark))
           (insert "]")

           (buffer-string))))))))

(ert-deftest turtles-grab-invisible-mark ()
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
           (turtles-grab-buffer test-buffer)
           (insert "]")
           (goto-char (mark))
           (insert "[")

           (buffer-string))))))))

(ert-deftest turtles-grab-buffer-position ()
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
      (ert-with-test-buffer (:name "grab")
        (turtles-grab-buffer test-buffer)
        (goto-char (turtles-pos-in-window-grab pos))
        (turtles-mark-point "<>")
        (delete-trailing-whitespace)
        (should  (equal
                  (concat
                   "line 0..\n"
                   "line 1..\n"
                   "line 2..\n"
                   "line 3..\n"
                   "line 4..\n"
                   "line 5..\n"
                   "line <>6..\n"
                   "line 7..\n"
                   "line 8..\n"
                   "line 9..\n")
                  (buffer-string)))))))

(ert-deftest turtles-grab-buffer-position-long-lines ()
  (turtles-ert-test)

  (let (test-buffer pos)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (setq-local truncate-lines nil)

      (dotimes (i 10)
        (insert "This is a ")
        (dotimes (i 20)
          (insert "long "))
        (insert (format "line %d.\n" i)))
      (goto-char (point-min))
      (search-forward "line 6")
      (setq pos (match-beginning 0))
      (goto-char (point-min))
      (ert-with-test-buffer (:name "grab")
        (turtles-grab-buffer test-buffer)
        (goto-char (turtles-pos-in-window-grab pos))
        (should (equal "line 6."
                       (buffer-substring
                        (point) (line-end-position))))))))

(ert-deftest turtles-grab-faces ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (insert (concat (propertize "Some" 'face 'success) " faces:\n"))
      (insert (concat "  " (propertize "highlight" 'face 'highlight) "\n"))
      (insert (concat "  " (propertize "error" 'face 'error) "\n"))
      (insert (concat "  " (propertize "link" 'face 'link) "\n"))
      (insert (concat "  " (propertize "success" 'face 'success) "\n"))

      (ert-with-test-buffer (:name "grab")
        (turtles-grab-buffer test-buffer '(highlight error success))

        (goto-char (point-min))

        (should (equal 'success (get-text-property (point-min) 'face)))

        (search-forward "faces")
        (should (equal nil (get-text-property (1- (point)) 'face)))
        (should (equal nil (get-text-property (1- (point)) 'font-lock-face)))

        (search-forward "highlight")
        (should (equal 'highlight (get-text-property (1- (point)) 'face)))
        (should (equal nil (get-text-property (1- (point)) 'font-lock-face)))

        (search-forward "error")
        (should (equal 'error (get-text-property (1- (point)) 'face)))
        (should (equal nil (get-text-property (1- (point)) 'font-lock-face)))

        (search-forward "link")
        ;; link isn't on the list of faces to be grabbed
        (should (equal nil (get-text-property (1- (point)) 'face)))
        (should (equal nil (get-text-property (1- (point)) 'font-lock-face)))

        (search-forward "success")
        (should (equal 'success (get-text-property (1- (point)) 'face)))
        (should (equal nil (get-text-property (1- (point)) 'font-lock-face)))))))

(ert-deftest turtles-grab-and-mark-faces ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (insert (concat (propertize "Some" 'face 'success) " faces:\n"))
      (insert (concat "  " (propertize "highlight" 'face 'highlight) "\n"))
      (insert (concat "  " (propertize "error" 'face 'error) "\n"))
      (insert (concat "  " (propertize "success" 'face 'success) "\n"))

      (should
       (equal
        "{Some} faces:\n  [highlight]\n  <<error>>\n  {success}"
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer test-buffer '(highlight error success))

           (delete-trailing-whitespace)
           (turtles-mark-text-with-faces '((highlight "[]")
                                            (error "<<>>")
                                            (success "{}")))

           (buffer-string))))))))

(ert-deftest turtles-grab-and-mark-faces-assymetric-markers ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (insert (concat (propertize "Some" 'face 'success) " faces:\n"))
      (insert (concat "  " (propertize "highlight" 'face 'highlight) "\n"))
      (insert (concat "  " (propertize "error" 'face 'error) "\n"))
      (insert (concat "  " (propertize "success" 'face 'success) "\n"))

      (should
       (equal
        "#s[Some] faces:\n  #h[highlight]\n  #e[error]\n  #s[success]"
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer test-buffer '(highlight error success))

           (delete-trailing-whitespace)
           (turtles-mark-text-with-faces '((highlight "#h[" "]")
                                            (error "#e[" "]")
                                            (success "#s[" "]")))

           (buffer-string))))))))

(ert-deftest turtles-grab-and-mark-faces-single-call ()
  (turtles-ert-test)

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)
      (insert (concat (propertize "Some" 'face 'success) " faces:\n"))
      (insert (concat "  " (propertize "highlight" 'face 'highlight) "\n"))
      (insert (concat "  " (propertize "error" 'face 'error) "\n"))
      (insert (concat "  " (propertize "success" 'face 'success) "\n"))

      (should
       (equal
        "Some faces:\n  [highlight]\n  e[error]\n  success"
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer test-buffer '(highlight error success))

           (delete-trailing-whitespace)
           (turtles-mark-text-with-face 'highlight "[]")
           (turtles-mark-text-with-face 'error "e[" "]")

           (buffer-string))))))))

(ert-deftest turtles-grab-margins ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (let ((testbuf (current-buffer)))
      (turtles-test-init-buffer)
      (setq-local left-margin-width 2)
      (setq-local right-margin-width 2)
      (insert (propertize "left" 'display '((margin left-margin) "|-")))
      (insert "Hello, \n")
      (insert (propertize "right" 'display '((margin right-margin) "-|")))
      (insert "world.\n")

      ;; Set the point just after Hello. This is tested to make sure
      ;; that the margin doesn't throw off turtles-pos-in-window-grab.
      (goto-char (point-min))
      (search-forward "Hello")

      (should (equal
               (concat "|-Hello<>,\n"
                       "  world.                                                                      -|\n")
               (with-temp-buffer
                 (turtles-grab-buffer testbuf nil 'margins)
                 (turtles-mark-point "<>")
                 (delete-trailing-whitespace)
                 (buffer-string)))))))

(ert-deftest turtles-grab-header-line ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (let ((testbuf (current-buffer)))
      (turtles-test-init-buffer)
      (setq-local header-line-format '("My very own header"))
      (insert "Hello, world\n")

      (should (equal
               "My very own header\n"
               (with-temp-buffer
                 (turtles-grab-header-line testbuf (current-buffer))
                 (delete-trailing-whitespace)
                 (buffer-string)))))))

(ert-deftest turtles-grab-mode-line ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (let ((testbuf (current-buffer)))
      (turtles-test-init-buffer)
      (setq-local mode-line-format '("My very own mode line"))
      (insert "Hello, world\n")

      (should (equal
               "My very own mode line\n"
               (with-temp-buffer
                 (turtles-grab-mode-line testbuf (current-buffer))
                 (delete-trailing-whitespace)
                 (buffer-string)))))))

(ert-deftest turtles-mark-point ()
  (ert-with-test-buffer ()
   (insert "Time is a drug. Too much of it kills you.")

   (goto-char (point-min))
   (search-forward "kills")

   (turtles-mark-point "<>")

   (should (equal "Time is a drug. Too much of it kills<> you."
                  (buffer-string)))))

(ert-deftest turtles-mark-region ()
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

(ert-deftest turtles-mark-region-swapped ()
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

(ert-deftest turtles-mark-region-twochars ()
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

(ert-deftest turtles-mark-region-opening-and-closing ()
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

(ert-deftest turtles-colors ()
  (turtles-ert-test)

  (let (orig-buf)
    (ert-with-test-buffer (:name "orig")
      (setq orig-buf (current-buffer))
      (turtles-test-init-buffer)
      (insert (propertize "green on red" 'face
                          `(:foreground "#00ff00" :background "#ff0000")))
      (insert "\n")
      (insert (propertize "yellow on blue" 'face
                          `(:foreground "#ffff00" :background "#0000ff")))
      (insert "\n")
      (ert-with-test-buffer (:name "capture")
        (turtles-grab-buffer orig-buf)
        (goto-char (point-min))

        (should (search-forward "green on red"))
        (goto-char (match-beginning 0))
        (should (equal (color-values "#00ff00")
                       (color-values (foreground-color-at-point))))
        (should (equal (color-values "#ff0000")
                       (color-values (background-color-at-point))))

        (should (search-forward "yellow on blue"))
        (goto-char (match-beginning 0))
        (should (equal (color-values "#ffff00")
                       (color-values (foreground-color-at-point))))
        (should (equal (color-values "#0000ff")
                       (color-values (background-color-at-point))))))))

(ert-deftest turtles-faces-from-color ()
  (ert-with-test-buffer ()
    (insert (propertize "red" 'font-lock-face '(:foreground "#ff0000" :background "#000000")))
    (insert " ")
    (insert (propertize "green" 'font-lock-face '(:foreground "#00ff00" :background "#000000")))
    (insert " ")
    (insert (propertize "blue" 'font-lock-face '(:foreground "#0000ff" :background "#000000")))
    (insert "\n")

    (insert (propertize "red bg" 'font-lock-face '(:foreground "#ffffff" :background "#ff0000")))
    (insert " ")
    (insert (propertize "green bg" 'font-lock-face '(:foreground "#ffffff" :background "#00ff00")))
    (insert " ")
    (insert (propertize "blue bg" 'font-lock-face '(:foreground "#ffffff" :background "#0000ff")))

    (turtles--faces-from-color '((error . (:foreground "#ff0000" :background "#000000"))
                                 (success . (:foreground "#00ff00" :background "#000000"))
                                 (link . (:foreground "#ffffff" :background "#0000ff"))))

    (turtles-mark-text-with-face 'error "[" "](error)")
    (turtles-mark-text-with-face 'success "[" "](success)")
    (turtles-mark-text-with-face 'link "[" "](link)")

    (should (equal (concat "[red](error) [green](success) blue\n"
                           "red bg green bg [blue bg](link)")
                   (buffer-string)))

    (should-not (get-text-property (point-min) 'font-lock-face))
    (should-not (next-single-property-change (point-min) 'font-lock-face))))

(ert-deftest turtles-faces-from-color-inexact ()
  (ert-with-test-buffer ()
    (insert (propertize "red" 'font-lock-face '(:foreground "#e10000" :background "black")))
    (insert " ")
    (insert (propertize "green" 'font-lock-face '(:foreground "#007100" :background "black")))
    (insert " ")
    (insert (propertize "blue" 'font-lock-face '(:foreground "#0000ef" :background "black")))

    (turtles--faces-from-color '((error . (:foreground "#ff0000" :background "#000000"))
                                 (success . (:foreground "#008000" :background "#000000"))
                                 (link . (:foreground "#0000ff" :background "#000000"))))

    (turtles-mark-text-with-face 'error "[" "](error)")
    (turtles-mark-text-with-face 'success "[" "](success)")
    (turtles-mark-text-with-face 'link "[" "](link)")

    (should (equal "[red](error) [green](success) [blue](link)"
                   (buffer-string)))))

(ert-deftest turtles-trim-buffer ()
  (ert-with-test-buffer ()
    (insert "  line 1         \n")
    (insert "line 2\t\t         \n")
    (insert "\n\n\n\n")

    (turtles-trim-buffer)

    (should (equal "  line 1\nline 2" (buffer-string)))))

(ert-deftest turtles-pass ()
  (turtles-ert-test)

  (should (equal 1 1)))

(ert-deftest turtles-fail ()
  :expected-result :failed
  (turtles-ert-test)

  (should (equal 1 2)))

(ert-deftest turtles-pass-send-body ()
  ;; File-name is missing, so the whole ert-test object must be sent
  ;; to the instance.
  (turtles--ert-test 'default nil nil)

  (should (equal 1 1)))

(ert-deftest turtles-fail-send-body ()
  :expected-result :failed
  (turtles--ert-test 'default nil nil)

  (should (equal 1 2)))

;; This test is used in manual tests to check out what happens to
;; buffers listed in failed tests.
(ert-deftest turtles-fail-with-buffer ()
  :expected-result :failed
  (turtles-ert-test)
  (ert-with-test-buffer (:name "mybuf")
    (insert "Test buffer")
    (goto-char (point-min))
    (search-forward "buffer")
    (goto-char (match-beginning 0))
    (should (equal 1 2))))

(ert-deftest turtles-recreate-buttons ()
  (define-button-type 'turtles-test-button 'action (lambda (button) (message "click")))

  (let* ((press-count 0)
         (button-text (with-temp-buffer
                        (insert "foobar")
                        (make-text-button 1 5 :type 'turtles-test-button)
                        (buffer-string)))
         (button-text2 (car (read-from-string (prin1-to-string button-text)))))
    (turtles--recreate-buttons button-text2)

    ;; If turtles--recreate-buttons doesn't work, these two properties
    ;; return symbols with the same name that are not actually eq.
    (should (eq (get-text-property 0 'category button-text)
                (get-text-property 0 'category button-text2)))))

(ert-deftest turtles-to-string-noarg ()
  (turtles-ert-test)
  (ert-with-test-buffer ()
    (insert "hello, world")
    (should (equal "hello, world" (turtles-to-string)))))

(ert-deftest turtles-to-string-buf ()
  (turtles-ert-test)

  (let (test-buffer)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (insert "hello, world")
      (with-temp-buffer
        (should (equal "hello, world" (turtles-to-string :buf test-buffer)))))))

(ert-deftest turtles-to-string-win ()
  (turtles-ert-test)

  (let (buf1 buf2)
    (ert-with-test-buffer (:name "buf1")
      (setq buf1 (current-buffer))
      (insert "hello, world")

      (ert-with-test-buffer (:name "buf2")
        (setq buf2 (current-buffer))
        (insert "foobar")

        (select-window (frame-root-window))
        (split-window-below nil)
        (pcase-let ((`(,win1 ,win2) (window-list)))
          (set-window-buffer win1 buf1)
          (set-window-buffer win2 buf2)

      (should (equal "hello, world" (turtles-to-string :win win1)))
      (should (equal "foobar" (turtles-to-string :win win2))))))))

(ert-deftest turtles-to-string-faces ()
  (turtles-ert-test)
  (ert-with-test-buffer ()
    (insert (propertize "hello" 'face 'error))
    (insert ", ")
    (insert (propertize "world" 'face 'success))
    (should (equal "[hello], {world}"
                   (turtles-to-string :faces '((error "[]")
                                                (success "{" "}")))))))

(ert-deftest turtles-to-string-region ()
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

(ert-deftest turtles-to-string-point ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert "baa, baa, black sheep, have you any wool?")
    (goto-char (point-min))
    (search-forward "black")

    (should
     (equal
      "baa, baa, black>< sheep, have you any wool?"
      (turtles-to-string :point "><")))))

(ert-deftest turtles-to-string-point-and-region ()
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

(ert-deftest turtles-with-grab-buffer-noarg ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert "hello, world")
    (turtles-with-grab-buffer ()
      (should (equal "hello, world"
                     (string-trim (buffer-string)))))))

(ert-deftest turtles-with-grab-buffer-result ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (insert "hello, world")
    (should (equal "hello, world"
                   (turtles-with-grab-buffer ()
                     (string-trim (buffer-string)))))))

(ert-deftest turtles-with-grab-buffer-buf ()
  (turtles-ert-test)

  (let (test-buffer)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (insert "hello, world")
      (with-temp-buffer
        (turtles-with-grab-buffer (:buf test-buffer)
          (should (equal "hello, world"
                         (string-trim (buffer-string)))))))))


(ert-deftest turtles-with-grab-buffer-win ()
  (turtles-ert-test)

  (let (buf1 buf2)
    (ert-with-test-buffer (:name "buf1")
      (setq buf1 (current-buffer))
      (insert "hello, world")

      (ert-with-test-buffer (:name "buf2")
        (setq buf2 (current-buffer))
        (insert "foobar")

        (select-window (frame-root-window))
        (split-window-below)
        (pcase-let ((`(,win1 ,win2) (window-list)))
          (set-window-buffer win1 buf1)
          (set-window-buffer win2 buf2)

          (turtles-with-grab-buffer (:win win1)
            (should (equal "hello, world"
                           (string-trim (buffer-string)))))
          (turtles-with-grab-buffer (:win win2)
            (should (equal "foobar"
                           (string-trim (buffer-string))))))))))

(ert-deftest turtles-with-grab-buffer-faces ()
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

(ert-deftest turtles-with-grab-buffer-faces-and-marks ()
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

(ert-deftest turtles-with-grab-buffer-margins ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
      (setq-local left-margin-width 2)
      (setq-local right-margin-width 2)
      (insert (propertize "left" 'display '((margin left-margin) "|-")))
      (insert (propertize "right" 'display '((margin right-margin) "-|")))
      (insert "Hello, world.")

      (turtles-with-grab-buffer (:margins t)
        (delete-trailing-whitespace)
        (should (equal "|-Hello, world.                                                               -|\n"
                       (buffer-string))))))

(ert-deftest turtles-with-grab-mode-line ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (setq-local mode-line-format "My mode line")
    (turtles-with-grab-buffer (:mode-line t)
      (delete-trailing-whitespace)
      (should (equal "My mode line\n" (buffer-string))))))


(ert-deftest turtles-with-grab-header-line ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (setq-local header-line-format "My header line")
    (turtles-with-grab-buffer (:header-line t)
      (delete-trailing-whitespace)
      (should (equal "My header line\n" (buffer-string))))))

(ert-deftest turtles-read-from-minibuffer ()
  (turtles-ert-test)

  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should
     (equal "hello"
            (turtles-read-from-minibuffer
                (read-from-minibuffer "Prompt: ")
              (execute-kbd-macro (kbd "hello"))
              (should (equal "Prompt: hello" (turtles-to-string)))
              (execute-kbd-macro (kbd "RET")))))))

;; Snippet shown in README.md
(ert-deftest turtles-hello-world ()
  (turtles-ert-test)             ;; Start a secondary Emacs instance
                                 ;; Everything below this point runs
                                 ;; in the secondary instance.

  (ert-with-test-buffer ()
    (insert "hello, ")           ;; Fill in the buffer
    (insert (propertize "the " 'invisible t))
    (insert "world!\n")

    (turtles-with-grab-buffer () ;; Grab the current buffer content
      (turtles-trim-buffer)      ;; Remove any extra newlines

      ;; Check the buffer content that was displayed
      (should (equal "hello, world!"
                     (buffer-string))))))

(ert-deftest turtles-pop-to-buffer-copy ()
  (let ((inst (turtles-get-instance 'default)))
    (should inst)
    (turtles-start-instance inst)

    (let ((remote-buf (turtles-io-call-method
                       (turtles-instance-conn inst)
                       'eval
                       '(with-current-buffer (generate-new-buffer "turtles-pop-to-buffer-copy")
                          (insert "This is my buffer.")
                          (goto-char (point-min))
                          (search-forward "buffer")
                          (goto-char (match-beginning 0))
                          (push-mark (match-end 0) 'nomsg nil)
                          (activate-mark)
                          (current-buffer)))))
      (should (consp remote-buf))
      (should (equal 'turtles-buffer (car remote-buf)))
      (should (equal 'default (plist-get (cdr remote-buf) :instance)))

      (let* ((turtles-pop-to-buffer-actions (list #'turtles-pop-to-buffer-copy))
             (buf (turtles-pop-to-buffer remote-buf)))
        (unwind-protect
            (progn
              (should (eq (selected-window) (get-buffer-window buf)))
              (with-current-buffer (window-buffer (selected-window))
                (should (eq inst turtles--original-instance))
                (should (string-prefix-p "[default] turtles-pop-to-buffer-copy" (buffer-name)))
                (turtles-mark-point "<>")
                (turtles-mark-region "[]")
                (should (equal "This is my <>[buffer]." (buffer-string)))))
          (kill-buffer buf))))))

(ert-deftest turtles-pop-to-buffer-embedded ()
  (let ((inst (turtles-get-instance 'default)))
    (should inst)
    (turtles-start-instance inst)

    (let ((remote-buf (turtles-io-call-method
                       (turtles-instance-conn inst)
                       'eval
                       '(with-current-buffer (generate-new-buffer "turtles-pop-to-buffer-embedded")
                          (insert "This is my buffer.")
                          (current-buffer)))))
      (should (consp remote-buf))
      (should (equal 'turtles-buffer (car remote-buf)))
      (should (equal 'default (plist-get (cdr remote-buf) :instance)))

      (let* ((turtles-pop-to-buffer-actions (list #'turtles-pop-to-buffer-embedded))
             (buf (turtles-pop-to-buffer remote-buf)))
        (should (eq buf (turtles-instance-term-buf inst)))
        (should (eq (selected-window) (get-buffer-window buf)))
        (let ((win (turtles-io-call-method
                    (turtles-instance-conn inst)
                    'eval
                    `(selected-window))))
          (should (consp win))
          (should (equal 'turtles-window (car win)))
          (should (equal (plist-get (cdr remote-buf) :name)
                         (plist-get (cdr win) :buffer))))))))

(ert-deftest turtles-pop-to-buffer-new-frame ()
  ;; There isn't much that can be tested in batch mode; the action is
  ;; just not available.
  (should (eq (alist-get 'window-system (frame-parameters))
              (turtles-pop-to-buffer-new-frame 'check nil nil))))
