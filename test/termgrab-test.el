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

(defun termgrab-test-init-buffer ()
  (setq-local truncate-lines t)
  (setq-local left-margin-width 0))

(ert-deftest termgrab-test-setup ()
  (termgrab-start-server)
  (should termgrab-tmux-proc)
  (should (string-prefix-p
           "grab: 1 windows"
           (with-temp-buffer
             (termgrab--tmux termgrab-tmux-proc (current-buffer) "list-sessions")
             (buffer-string))))

  (let ((grabbed (termgrab-grab-frame-to-string)))
    (should (stringp grabbed))))

(ert-deftest termgrab-test-setup-buffer ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (termgrab-test-init-buffer)
    (insert "test buffer")

    (should termgrab-frame)
    (should (frame-live-p termgrab-frame))

    (termgrab-setup-buffer)

    (let ((grabbed (termgrab-grab-frame-to-string)))
      (should (stringp grabbed))
      (should (string-match-p "test buffer" grabbed)))))

(ert-deftest termgrab-test-setup-other-buffer ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (termgrab-test-init-buffer)
    (insert "test buffer")

    (let ((buf (current-buffer)))
      (with-current-buffer "*scratch*"
      (termgrab-setup-buffer buf)))

    (let ((grabbed (termgrab-grab-frame-to-string)))
      (should (stringp grabbed))
      (should (string-match-p "test buffer" grabbed)))))

(ert-deftest termgrab-test-grab-buffer-head ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (termgrab-test-init-buffer)
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
             (termgrab-grab-buffer-to-string (current-buffer))))))

(ert-deftest termgrab-test-grab-buffer-tail ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (termgrab-test-init-buffer)
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
             (termgrab-grab-buffer-to-string (current-buffer))))))

(ert-deftest termgrab-test-grab-buffer-full-lines ()
  (termgrab-start-server)
  (ert-with-test-buffer ()
    (termgrab-test-init-buffer)
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
             (termgrab-grab-buffer-to-string (current-buffer))))))

(ert-deftest termgrab-test-grab-window-horiz-center ()
  (termgrab-start-server)
  (let (buf1 buf2 center-win)
    (ert-with-test-buffer (:name "buf1")
      (termgrab-test-init-buffer)
      (setq buf1 (current-buffer))
      (dotimes (i 40)
        (insert (format "line %d%s\n" i (make-string 80 ?-))))
      (goto-char (point-min))

      (ert-with-test-buffer (:name "buf2")
        (termgrab-test-init-buffer)
        (setq buf2 (current-buffer))
        (dotimes (i 40)
          (insert (make-string 80 ?x)))
        (goto-char (point-min))
        
        (set-window-buffer (termgrab-root-window) buf2)
        (setq center-win (split-window-below 5 (termgrab-root-window)))
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
          (termgrab-grab-window-to-string center-win)))))))

(ert-deftest termgrab-test-grab-window-vert-center ()
  (termgrab-start-server)
  (let (buf1 buf2 center-win)
    (ert-with-test-buffer (:name "buf1")
      (termgrab-test-init-buffer)
      (setq buf1 (current-buffer))
      (dotimes (i 40)
        (insert (format "line %d%s\n" i (make-string 80 ?-))))
      (goto-char (point-min))

      (ert-with-test-buffer (:name "buf2")
        (termgrab-test-init-buffer)

        (setq buf2 (current-buffer))
        (insert "begin")
        (dotimes (i 40)
          (insert "\n"))
        (insert "end")
        (goto-char (point-min))

        (set-window-buffer (termgrab-root-window) buf2)
        (setq center-win (split-window-right 20 (termgrab-root-window)))
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
          (termgrab-grab-window-to-string center-win)))))))

(ert-deftest termgrab-test-grab-window-vert-center-empty-buffer ()
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
  :expected-result (if noninteractive :failed :passed)
  (termgrab-start-server)
  (let (buf1 buf2 center-win)
    (ert-with-test-buffer (:name "buf1")
      (termgrab-test-init-buffer)
      (setq buf1 (current-buffer))
      (dotimes (i 40)
        (insert (format "line %d%s\n" i (make-string 80 ?-))))
      (goto-char (point-min))

      (ert-with-test-buffer (:name "buf2")
        (termgrab-test-init-buffer)

        (setq buf2 (current-buffer))
        (insert "mostly empty\n\n.")
        (goto-char (point-min))

        (set-window-buffer (termgrab-root-window) buf2)
        (setq center-win (split-window-right 20 (termgrab-root-window)))
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
          (termgrab-grab-window-to-string center-win)))))))

(ert-deftest termgrab-test-colors ()
  (let (orig-buf capture-buf)
    (termgrab-start-server)
    (ert-with-test-buffer (:name "orig")
      (setq orig-buf (current-buffer))
      (termgrab-test-init-buffer)
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
        (termgrab-grab-buffer-into orig-buf capture-buf)
        (goto-char (point-min))

        (should (search-forward "green on red"))
        (goto-char (1- (point)))
        (should (equal (face-foreground 'ansi-color-green) (foreground-color-at-point)))
        (should (equal (face-background 'ansi-color-red) (background-color-at-point)))

        (should (search-forward "yellow on blue"))
        (goto-char (1- (point)))
        (should (equal (face-foreground 'ansi-color-yellow) (foreground-color-at-point)))
        (should (equal (face-background 'ansi-color-blue) (background-color-at-point)))))))

;;; termgrab-test.el ends here
