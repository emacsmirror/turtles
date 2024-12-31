.. _tut:

Tutorial
========


.. code-block:: elisp

  (require 'ert)
  (require 'ert-x)
  (require 'turtles)

.. _tut_grab:

Screen Grab
-----------

.. code-block:: elisp

  (ert-deftest turtles-examples-hello-world ()
     ;; Start a secondary Emacs instance
    (turtles-ert-test)
  
    ;; From this point, everything runs in the secondary instance.
    (ert-with-test-buffer ()
      (insert "hello, ") ; Fill in the buffer
      (insert (propertize "the " 'invisible t))
      (insert "world!\n")
  
      (turtles-with-grab-buffer () ; Grab current buffer content
        (should (equal "hello, world!"
                       (buffer-string))))))                

.. code-block:: elisp

  (ert-deftest turtles-examples-test-hideshow ()
    (turtles-ert-test)
  
    (ert-with-test-buffer ()
      (insert "(defun test-1 ()\n")
      (insert " (message \"test, the first\"))\n")
      (insert "(defun test-2 ()\n")
      (insert " (message \"test, the second\"))\n")
      (insert "(defun test-3 ()\n")
      (insert " (message \"test, the third\"))\n")
  
      (emacs-lisp-mode)
      (hs-minor-mode)
  
      (goto-char (point-min))
      (search-forward "test-2")
      (hs-hide-block)
      (turtles-with-grab-buffer (:name "hide test-2")
        (should (equal (concat
                        "(defun test-1 ()\n"
                        " (message \"test, the first\"))\n"
                        "(defun test-2 ()...)\n"
                        "(defun test-3 ()\n"
                        " (message \"test, the third\"))")
                       (buffer-string))))
  
      (hs-hide-all)
      (turtles-with-grab-buffer (:name "hide all")
        (should (equal (concat
                        "(defun test-1 ()...)\n"
                        "(defun test-2 ()...)\n"
                        "(defun test-3 ()...)")
                       (buffer-string))))))

.. _tut_debug:

Debugging
---------

.. _tut_minibuffer:

Minibuffer
----------

.. code-block:: elisp

  (ert-deftest turtles-examples-test-completing-read ()
    (turtles-ert-test)
  
    (ert-with-test-buffer ()
      (let ((completing-read-function #'completing-read-default))
        (turtles-with-minibuffer
            (should
             (equal "Choice B"
                    (completing-read "Choose: " '("Choice A" "Choice B") nil t)))
  
          (turtles-with-grab-buffer (:name "initial prompt" :point "<>")
            (should (equal "Choose: <>" (buffer-string))))
  
          (execute-kbd-macro (kbd "Ch TAB"))
          (turtles-with-grab-buffer (:name "completion" :point "<>")
            (should (equal "Choose: Choice <>" (buffer-string))))
  
          (execute-kbd-macro (kbd "B"))))))

.. _tut_faces:   

Faces
-----

.. code-block:: elisp

  (ert-deftest turtles-examples-test-isearch ()
    (turtles-ert-test)
  
    (ert-with-test-buffer ()
      (let ((testbuf (current-buffer)))
        (select-window (display-buffer testbuf))
        (delete-other-windows)
  
        (insert "Baa, baa, black sheep, have you any wool?")
        (goto-char (point-min))
  
        (turtles-with-minibuffer
            (isearch-forward nil 'no-recursive-edit)
  
          (turtles-input-keys "baa")
          (turtles-with-grab-buffer (:minibuffer t)
            (should (equal "I-search: baa" (buffer-string))))
          (turtles-with-grab-buffer (:buf testbuf :name "match 1" :faces '((isearch . "[]")))
            (should (equal "[Baa], baa, black sheep, have you any wool?"
                           (buffer-string))))
  
          (turtles-input-keys "\C-s")
          (turtles-with-grab-buffer (:buf testbuf :name "match 2" :faces '((isearch . "[]")))
            (should (equal "Baa, [baa], black sheep, have you any wool?"
                           (buffer-string))))
  
          (isearch-done))
  
        (turtles-with-grab-buffer (:name "final position" :point "<>")
          (should (equal "Baa, baa<>, black sheep, have you any wool?"
                         (buffer-string)))))))
                  

.. _tut_colors:   

Colors
------


                         
