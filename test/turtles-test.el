;; turtles-test.el --- Test turtles.el -*- lexical-binding: t -*-

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

(require 'button)
(require 'compat)
(require 'ert)
(require 'ert-x)

(require 'turtles)
(require 'turtles-io)

(defun turtles-test-init-buffer ()
  (setq-local truncate-lines t)
  (setq-local left-margin-width 0))

(turtles-ert-deftest turtles-grab-frame ()
  (ert-with-test-buffer ()
    (turtles-display-buffer-full-frame (current-buffer))
    (insert "De Chelonian Mobile")
    (ert-with-test-buffer ()
      (turtles-grab-frame)
      (goto-char (point-min))
      (should (search-forward "De Chelonian Mobile")))))

(turtles-ert-deftest turtles-grab-frame-with-window ()
  (let (bufa bufb)
    (ert-with-test-buffer (:name "a")
      (setq bufa (current-buffer))
      (insert "This is buffer A")

      (ert-with-test-buffer (:name "b")
        (setq bufb (current-buffer))
        (insert "This is buffer B")
        (goto-char (point-min))
        (search-forward "buffer")

        (turtles-display-buffer-full-frame bufa)
        (select-window (frame-root-window))
        (set-window-buffer (split-window-below) bufb)
        (select-window (get-buffer-window bufa))

        (ert-with-test-buffer (:name "grab")
          ;; The window of buffer B must be the one selected during
          ;; the grab, so that's where the pointer will be.
          (turtles-grab-frame (get-buffer-window bufb))
          (should (eq (get-buffer-window bufa) (selected-window)))
          (insert "<>")
          (should (equal "This is buffer<> B"
                         (buffer-substring (line-beginning-position)
                                           (line-end-position))))
          (goto-char (point-min))
          (should (search-forward "This is buffer A")))))))

(ert-deftest turtles-grab-frame-from-server ()
  (should-error (with-temp-buffer
                  (turtles-grab-frame))))

(turtles-ert-deftest turtles-grab-buffer-head ()
  (ert-with-test-buffer ()
    (turtles-test-init-buffer)
    (dotimes (i 100)
      (insert (format "line %d\n" i)))
    (goto-char (point-min))
    (turtles-with-grab-buffer ()
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
         "line 17\n"
         "line 18\n"
         "line 19\n"
         "line 20\n"
         "line 21")
        (buffer-string))))))

(turtles-ert-deftest turtles-grab-buffer-tail ()
  (ert-with-test-buffer ()
    (turtles-test-init-buffer)
    (dotimes (i 100)
      (insert (format "line %d\n" i)))
    (goto-char (point-max))
    (turtles-with-grab-buffer ()
      (should
       (equal
        (concat
         "line 89\n"
         "line 90\n"
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

(turtles-ert-deftest turtles-grab-buffer-full-lines ()
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
         "line 17------------------------------------------------------------------------$\n"
         "line 18------------------------------------------------------------------------$\n"
         "line 19------------------------------------------------------------------------$\n"
         "line 20------------------------------------------------------------------------$\n"
         "line 21------------------------------------------------------------------------$")
        (buffer-string))))))

(turtles-ert-deftest turtles-grab-window-horiz-center ()
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
          (turtles-grab-window center-win)
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

(turtles-ert-deftest turtles-grab-window-vert-center ()
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
          (turtles-grab-window center-win)
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
             "line 17-----------$\n"
             "line 18-----------$\n"
             "line 19-----------$\n"
             "line 20-----------$\n"
             "line 21-----------$\n")
            (buffer-string))))))))

(turtles-ert-deftest turtles-grab-window-vert-center-empty-buffer ()
  (let (buf1 buf2 center-win)
    (ert-with-test-buffer (:name "buf1")
      (turtles-test-init-buffer)
      (setq buf1 (current-buffer))
      (dotimes (i 40)
        (insert (format "line %d%s\n" i (make-string 80 ?-))))
      (goto-char (point-min))

      (ert-with-test-buffer (:name "buf2")
        (turtles-test-init-buffer)

        ;; The buffer on the right being empty causes display issues
        ;; in a noninteractive Emacs instance. This is why this test
        ;; exists.
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
          (turtles-grab-window center-win)
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
             "line 17-----------$\n"
             "line 18-----------$\n"
             "line 19-----------$\n"
             "line 20-----------$\n"
             "line 21-----------$\n")
            (buffer-string))))))))

(turtles-ert-deftest turtles-grab-point ()
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

(turtles-ert-deftest turtles-grab-point-bottom-windows ()
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
                "line 7..\n"
                "line 8..\n"
                "line 9..")
        (string-trim
         (ert-with-test-buffer (:name "grab")
           (turtles-grab-buffer test-buffer)
           (turtles-mark-point "<>")
           (buffer-string))))))))

(turtles-ert-deftest turtles-grab-faces ()
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

        (search-forward "highlight")
        (should (equal 'highlight (get-text-property (1- (point)) 'face)))

        (search-forward "error")
        (should (equal 'error (get-text-property (1- (point)) 'face)))

        (search-forward "link")
        ;; link isn't on the list of faces to be grabbed
        (should (equal nil (get-text-property (1- (point)) 'face)))

        (search-forward "success")
        (should (equal 'success (get-text-property (1- (point)) 'face)))))))

(turtles-ert-deftest turtles-grab-and-mark-faces ()
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

(turtles-ert-deftest turtles-grab-and-mark-faces-assymetric-markers ()
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

(turtles-ert-deftest turtles-grab-and-mark-faces-single-call ()
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

(turtles-ert-deftest turtles-grab-and-mark-faces-not-extend ()
  ;; :extend became available in Emacs 27.1. Under Emacs 26, all faces
  ;; are treated as having :extend t.
  (skip-unless (>= emacs-major-version 27))

  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)

      (should-not (face-attribute 'error :extend nil 'default))

      (insert "error: ")
      (insert (propertize "line1  \nline2\nline3" 'face 'error))
      (insert ".\n")

      (should
       (equal
        (concat
         "error: {line1   }\n"
         "{line2 }\n"
         "{line3}.")
        (ert-with-test-buffer (:name "grab")
          (turtles-grab-buffer test-buffer '(error))
          (turtles-mark-text-with-face 'error "{}")
          (turtles-trim-buffer)
          (buffer-string)))))))

(turtles-ert-deftest turtles-grab-and-mark-faces-extend ()
  (let ((test-buffer))
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (turtles-test-init-buffer)

      ;; Face with :extend t are treated differently both by Emacs
      ;; when displaying and by Turtles when grabbing and marking.
      (when (>= emacs-major-version 27)
        (should (face-attribute 'region :extend nil 'default)))

      (insert "region: ")
      (insert (propertize "line1  \nline2\nline3" 'face 'region))
      (insert ".\n")
      (insert "region-end-with-nl: ")
      (insert (propertize "line1  \nline2\nline3" 'face 'region))
      (insert "\n")
      (insert "region-end-at-eob: ")
      (insert (propertize "line1  \nline2\nline3" 'face 'region))

      (should
       (equal
        (concat
         "region: [line1\n"
         "line2\n"
         "line3].\n"
         "region-end-with-nl: [line1\nline2\nline3]\n"
         "region-end-at-eob: [line1\nline2\nline3]")
        (ert-with-test-buffer (:name "grab")
          (turtles-grab-buffer test-buffer '(region))
          (turtles-mark-text-with-face 'region "[]")
          (turtles-trim-buffer)
          (buffer-string)))))))

(turtles-ert-deftest turtles-grab-margins ()
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

(turtles-ert-deftest turtles-grab-header-line ()
  (ert-with-test-buffer ()
    (let ((testbuf (current-buffer)))
      (turtles-test-init-buffer)
      (setq-local header-line-format '("My very own header"))
      (insert "Hello, world\n")

      (should (equal
               "My very own header\n"
               (with-temp-buffer
                 (turtles-grab-header-line testbuf)
                 (delete-trailing-whitespace)
                 (buffer-string)))))))

(turtles-ert-deftest turtles-grab-mode-line ()
  (ert-with-test-buffer ()
    (let ((testbuf (current-buffer)))
      (turtles-test-init-buffer)
      (setq-local mode-line-format '("My very own mode line"))
      (insert "Hello, world\n")

      (should (equal
               "My very own mode line\n"
               (with-temp-buffer
                 (turtles-grab-mode-line testbuf)
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

(turtles-ert-deftest turtles-colors ()
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
    (insert (propertize "red" 'face '(:foreground "#ff0000" :background "#000000")))
    (insert " ")
    (insert (propertize "green" 'face '(:foreground "#00ff00" :background "#000000")))
    (insert " ")
    (insert (propertize "blue" 'face '(:foreground "#0000ff" :background "#000000")))
    (insert "\n")

    (insert (propertize "red bg" 'face '(:foreground "#ffffff" :background "#ff0000")))
    (insert " ")
    (insert (propertize "green bg" 'face '(:foreground "#ffffff" :background "#00ff00")))
    (insert " ")
    (insert (propertize "blue bg" 'face '(:foreground "#ffffff" :background "#0000ff")))

    (turtles--faces-from-color '((error . (:foreground "#ff0000" :background "#000000"))
                                 (success . (:foreground "#00ff00" :background "#000000"))
                                 (link . (:foreground "#ffffff" :background "#0000ff"))))

    (turtles-mark-text-with-face 'error "[" "](error)")
    (turtles-mark-text-with-face 'success "[" "](success)")
    (turtles-mark-text-with-face 'link "[" "](link)")

    (should (equal (concat "[red](error) [green](success) blue\n"
                           "red bg green bg [blue bg](link)")
                   (buffer-string)))))

(ert-deftest turtles-faces-from-color-inexact ()
  (ert-with-test-buffer ()
    (insert (propertize "red" 'face '(:foreground "#e10000" :background "black")))
    (insert " ")
    (insert (propertize "green" 'face '(:foreground "#007100" :background "black")))
    (insert " ")
    (insert (propertize "blue" 'face '(:foreground "#0000ef" :background "black")))

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

(turtles-ert-deftest turtles-pass ()
  (should (equal 1 1)))

(turtles-ert-deftest turtles-fail ()
  :expected-result :failed

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
(turtles-ert-deftest turtles-fail-with-buffer ()
  :expected-result :failed
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

(turtles-ert-deftest turtles-to-string-noarg ()  (ert-with-test-buffer ()
    (insert "hello, world")
    (should (equal "hello, world" (turtles-to-string)))))

(turtles-ert-deftest turtles-to-string-buf ()
  (let (test-buffer)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (insert "hello, world")
      (with-temp-buffer
        (should (equal "hello, world" (turtles-to-string :buf test-buffer)))))))

(turtles-ert-deftest turtles-to-string-win ()
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

(turtles-ert-deftest turtles-to-string-faces ()  (ert-with-test-buffer ()
    (insert (propertize "hello" 'face 'error))
    (insert ", ")
    (insert (propertize "world" 'face 'success))
    (should (equal "[hello], {world}"
                   (turtles-to-string :faces '((error "[]")
                                                (success "{" "}")))))))

(turtles-ert-deftest turtles-to-string-point ()
  (ert-with-test-buffer ()
    (insert "baa, baa, black sheep, have you any wool?")
    (goto-char (point-min))
    (search-forward "black")

    (should
     (equal
      "baa, baa, black>< sheep, have you any wool?"
      (turtles-to-string :point "><")))))

(turtles-ert-deftest turtles-with-grab-buffer-noarg ()
  (ert-with-test-buffer ()
    (insert "hello, world")
    (turtles-with-grab-buffer ()
      (should (equal "hello, world"
                     (string-trim (buffer-string)))))))

(turtles-ert-deftest turtles-with-grab-buffer-result ()
  (ert-with-test-buffer ()
    (insert "hello, world")
    (should (equal "hello, world"
                   (turtles-with-grab-buffer ()
                     (string-trim (buffer-string)))))))

(turtles-ert-deftest turtles-with-grab-buffer-buf ()
  (let (test-buffer)
    (ert-with-test-buffer ()
      (setq test-buffer (current-buffer))
      (insert "hello, world")
      (with-temp-buffer
        (turtles-with-grab-buffer (:buf test-buffer)
          (should (equal "hello, world"
                         (string-trim (buffer-string)))))))))


(turtles-ert-deftest turtles-with-grab-buffer-win ()
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

(turtles-ert-deftest turtles-with-grab-buffer-faces ()
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

(turtles-ert-deftest turtles-with-grab-buffer-faces-and-marks ()
  (ert-with-test-buffer ()
    (insert (propertize "error" 'face 'error))
    (insert ", ")
    (insert (propertize "success" 'face 'success))
    (turtles-with-grab-buffer (:faces '(success (error . "{}") ))
      (should (equal "{error}, success" (buffer-string)))
      (goto-char (point-min))
      (search-forward "success")
      (should (equal 'success (get-text-property (1- (point)) 'face))))))

(turtles-ert-deftest turtles-with-grab-buffer-margins ()
  (ert-with-test-buffer ()
      (setq-local left-margin-width 2)
      (setq-local right-margin-width 2)
      (insert (propertize "left" 'display '((margin left-margin) "|-")))
      (insert (propertize "right" 'display '((margin right-margin) "-|")))
      (insert "Hello, world.")

      (turtles-with-grab-buffer (:margins t)
        (delete-trailing-whitespace)
        (should (equal "|-Hello, world.                                                               -|"
                       (buffer-string))))))

(turtles-ert-deftest turtles-with-grab-buffer-point ()
  (ert-with-test-buffer ()
    (insert "baa, baa, black sheep, have you any wool?")
    (goto-char (point-min))
    (search-forward "black")

    (turtles-with-grab-buffer (:point "><")
      (should
       (equal
        "baa, baa, black>< sheep, have you any wool?"
        (buffer-string))))))

(turtles-ert-deftest turtles-with-grab-mode-line ()
  (ert-with-test-buffer ()
    (setq-local mode-line-format "My mode line")
    (turtles-with-grab-buffer (:mode-line t)
      (should (equal "My mode line" (buffer-string))))))


(turtles-ert-deftest turtles-with-grab-header-line ()
  (ert-with-test-buffer ()
    (setq-local header-line-format "My header line")
    (turtles-with-grab-buffer (:header-line t)
      (should (equal "My header line" (buffer-string))))))

(turtles-ert-deftest turtles-with-minibuffer ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should
     (equal "hello"
            (turtles-with-minibuffer
                (read-from-minibuffer "Prompt: ")
              (execute-kbd-macro (kbd "hello"))
              (should (equal "Prompt: hello" (turtles-to-string)))
              (execute-kbd-macro (kbd "RET")))))))

(turtles-ert-deftest turtles-larger-terminal ( :instance 'larger)
  ;; This makes sure that screen grabbing isn't confused by a larger
  ;; terminal size.
  (ert-with-test-buffer ()
    (insert "hello, world!\n")
    (turtles-with-grab-buffer ()
      (should (equal "hello, world!" (buffer-string))))))

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
                (when (region-active-p)
                  (save-excursion
                    (goto-char (mark))
                    (insert "<m>")))
                (should (equal "This is my <>buffer<m>." (buffer-string)))))
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

(ert-deftest turtles-pop-to-buffer-other-frame ()
  ;; There isn't much that can be tested in batch mode; the action is
  ;; just not available.
  (should (eq (alist-get 'window-system (frame-parameters))
              (turtles-pop-to-buffer-other-frame :check nil nil))))

(turtles-ert-deftest turtles-pop-to-buffer-interactive ()
  (let ((inst (turtles-start-instance 'default))
        (buf (turtles-instance-eval
              'default
              '(with-current-buffer
                   (generate-new-buffer "*pop-to-buffer-test*")
                 (insert "foo bar")
                 (current-buffer))))
        (turtles-pop-to-buffer-actions
         (list #'turtles-pop-to-buffer-embedded
               #'turtles-pop-to-buffer-copy))
        (inhibit-message t))

    (turtles-with-minibuffer
        (let ((completion-cycle-threshold 0))
          (turtles-pop-to-buffer buf))

      (turtles-with-grab-buffer (:name "initial")
          (should (equal "Display buffer:" (buffer-string))))

      (minibuffer-complete)
      (minibuffer-complete)
      (turtles-with-grab-buffer (:buf "*Completions*")
        (goto-char (point-max))
        (forward-line -1)
        (should (equal (concat "copy Display a copy of the instance buffer.\n"
                               "embedded Display buffer in the terminal buffer.")
                       (buffer-substring (point) (point-max))))

        (execute-kbd-macro (kbd "copy"))))

    ;; pop-to-buffer should have made a copy. Let's check it.
    (with-current-buffer (window-buffer (selected-window))
      (should (string-prefix-p "[default] *pop-to-buffer-test*" (buffer-name)))
      (should (equal "foo bar" (buffer-string)))
      (kill-buffer (current-buffer)))))

(turtles-ert-deftest turtles-pop-to-buffer-interactive-lambdas ()
  (let* ((inst (turtles-start-instance 'default))
         (buf (turtles-instance-eval
               'default
               '(with-current-buffer
                    (generate-new-buffer "*pop-to-buffer-test*")
                  (insert "foo bar")
                  (current-buffer))))
         (calls nil)
         (turtles-pop-to-buffer-actions
          (list
           (lambda (action &rest _args)
             "My Lambda 1."
             (when (eq :display action)
               (push 'lambda-1 calls))
             t)
           (lambda (action &rest _args)
             "My Lambda 2."
             (when (eq :display action)
               (push 'lambda-2 calls))
             t)))
         (inhibit-message t))

    (turtles-with-minibuffer
        (let ((completion-cycle-threshold 0))
          (turtles-pop-to-buffer buf))
      
      (turtles-with-grab-buffer (:name "initial")
        (should (equal "Display buffer:" (buffer-string))))

      (minibuffer-complete)
      (minibuffer-complete)
      (turtles-with-grab-buffer (:buf "*Completions*")
        (goto-char (point-max))
        (forward-line -1)
        (should (equal (concat "lambda-1 My Lambda 1.\n"
                               "lambda-2 My Lambda 2.")
                       (buffer-substring (point) (point-max)))))
      
      (execute-kbd-macro (kbd "1"))
      (turtles-with-grab-buffer (:name "choice")
        (should (equal "Display buffer: lambda-1" (buffer-string)))))

    (should (equal '(lambda-1) calls))))

(ert-deftest turtles--split-minibuffer-body ()
  (should
   (equal
    '(list
      (lambda ()
        (should (equal "Prompt:" (turtles-to-string)))
        (turtles--send-input (kbd "he")))
      (lambda nil
        (should (equal "Prompt: he" (turtles-to-string)))
        (turtles--send-input (kbd "llo")))
      (lambda nil
        (should (equal "Prompt: hello" (turtles-to-string)))
        (turtles--with-minibuffer-body-end)))
    (turtles--split-with-minibuffer-body
     `((should (equal "Prompt:" (turtles-to-string)))
       :keys "he"
       (should (equal "Prompt: he" (turtles-to-string)))
       :keys "llo"
       (should (equal "Prompt: hello" (turtles-to-string))))))))

(turtles-ert-deftest turtles-with-minibuffer-with-keys ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should
     (equal
      "hello"
      (turtles-with-minibuffer
          (read-from-minibuffer "Prompt: ")

        (should (equal "Prompt:" (turtles-to-string)))
        :keys "hep"
        (should (equal "Prompt: hep" (turtles-to-string)))
        :keys "DEL llo"
        (should (equal "Prompt: hello" (turtles-to-string))))))))

(turtles-ert-deftest turtles-with-minibuffer-with-command ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should
     (equal
      "foo, bar, "
      (turtles-with-minibuffer
          (read-from-minibuffer "Prompt: ")

        (should (equal "Prompt:" (turtles-to-string)))
        :keys "foo, SPC bar, SPC foo, SPC foo"
        (should (equal "Prompt: foo, bar, foo, foo" (turtles-to-string)))

        :command #'backward-kill-word
        (should (equal "Prompt: foo, bar, foo," (turtles-to-string)))

        (set-transient-map (let ((map (make-sparse-keymap)))
                             (define-key map (kbd "<f62>") #'backward-kill-word)
                             map))

        :command #'backward-kill-word
        (should (equal "Prompt: foo, bar," (turtles-to-string))))))))

(turtles-ert-deftest turtles-with-minibuffer-with-command-lambda ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should
     (equal
      "hello world"
      (turtles-with-minibuffer
          (read-from-minibuffer "Prompt: ")

        :command (lambda ()
                   (interactive)
                   (insert "hello world"))
        (should (equal "Prompt: hello world" (turtles-to-string))))))))

(turtles-ert-deftest turtles-with-minibuffer-with-command-with-keybinding ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should
     (equal
      "C-c t"
      (turtles-with-minibuffer
          (read-from-minibuffer "Prompt: ")

        :command-with-keybinding
        "C-c t" (lambda ()
                  (interactive)
                  (insert (key-description (this-command-keys))))
        (should (equal "Prompt: C-c t" (turtles-to-string))))))))

(turtles-ert-deftest turtles-with-minibuffer-with-events ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should
     (equal
      "hello"
      (turtles-with-minibuffer
          (read-from-minibuffer "Prompt: ")

        :events (kbd "hello")
        (should (equal "Prompt: hello" (turtles-to-string))))))))

(turtles-ert-deftest turtles-read-from-minibuffer-with-typo ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should-error
     (turtles-with-minibuffer
         (read-from-minibuffer "Prompt: ")
       :typo (kbd "hello")))))

(turtles-ert-deftest turtles-with-minibuffer-early-return ()
  (let ((reached nil))
    (should-error (turtles-with-minibuffer
                      nil
                    (message "notreached")
                    (setq reached t)))
    (should-not reached)))

(turtles-ert-deftest turtles-with-minibuffer-pile-up-errors ()
  ;; This test makes sure that a failures from the body of
  ;; turtles-with-minibuffer is the one that's reported when both
  ;; body and read sections fail.
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (should
     (equal "first"
            (catch 'thrown
              (turtles-with-minibuffer
                  (progn
                    (read-from-minibuffer "Prompt: ")
                    (throw 'thrown "second"))
                :keys "boo"
                (turtles-with-grab-buffer ()
                  (throw 'thrown "first"))))))))

(ert-deftest turtles-read-from-minibuffer-not-in-an-instance ()
  ;; turtles-read-from-minibuffer only works in interactive mode.
  :expected-result (if noninteractive :failed :passed)
  (should (equal "ok"
                 (turtles-with-minibuffer
                     (read-from-minibuffer "Prompt: ")
                   (execute-kbd-macro "ok")))))

(ert-deftest turtles-with-minibuffer-not-in-an-turtles-test ()
  ;; In contrast to the previous test, using :keys outside of an
  ;; instance cannot work.
  (should-error
   (turtles-with-minibuffer
       (read-from-minibuffer "Prompt: ")
     :keys "ok")))

(turtles-ert-deftest turtles-term-truecolor ()
  (skip-unless (>= emacs-major-version 29))
  ;; Truecolor in term.el became available in Emacs 29.1. Before that,
  ;; term was limited to just 16 (Emacs 28) and even 8 (Emacs 26)
  ;; colors.

  (should (equal 16777216 (display-color-cells)))

  (ert-with-test-buffer ()
    (insert (propertize "yellow" 'face '(:foreground "#faf32c" :background "#3a3913")))
    (insert " ")
    (insert (propertize "submarine" 'face '(:foreground "#276ce2" :background "#0c1526")))
    (insert "\n")

    (turtles-with-grab-buffer ()
      (turtles-trim-buffer)

      (goto-char (point-min))
      (search-forward "yellow")
      (goto-char (match-beginning 0))
      (should (string-equal-ignore-case "#faf32c" (foreground-color-at-point)))
      (should (string-equal-ignore-case "#3a3913" (background-color-at-point)))

      (search-forward "submarine")
      (goto-char (match-beginning 0))
      (should (string-equal-ignore-case "#276ce2" (foreground-color-at-point)))
      (should (string-equal-ignore-case "#0c1526" (background-color-at-point))))))

(ert-deftest turtles-ert-test-body-split ()
  (should (equal '(nil . ((should t) (should-not nil)))
                 (turtles--ert-test-body-split '((should t)
                                                 (should-not nil)))))

  (should (equal '(("docstring") . ((should t)))
                 (turtles--ert-test-body-split '("docstring"
                                                 (should t)))))

  (should (equal '((:tags (tags) :expected-result :failed) .
                   ((should t)))
                 (turtles--ert-test-body-split '(:tags (tags) :expected-result :failed
                                                 (should t)))))

  (should (equal '(("should fail" :expected-result :failed) .
                   ((should t)))
                 (turtles--ert-test-body-split '("should fail"
                                                 :expected-result :failed
                                                 (should t))))))
  
