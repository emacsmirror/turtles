.. _tut:

Tutorial
========

Turtles is a collection of tools for writing ERT tests that look at
the terminal showing an interactive Emacs instance from within that
instance. That is, you can setup buffer content and windows and check
how it looks like to a user running Emacs in a terminal.

Such ERT tests can run in batch mode as well an in interactive mode.

To make that work, Turtles starts a secondary Emacs instance from
within a terminal buffer. Tests run in the secondary Emacs instance
and their result communicated to the primary Emacs instance. Whenever
needed, the primary instance grab the screen and provides the result,
that is, a terminal screen with colors and cursor position, to the
secondary instance.

To get started, :ref:`install turtles <install>`, create a new ERT
test file with:

.. code-block:: elisp

  ;; -*- lexical-binding: t -*-

  (require 'ert)
  (require 'ert-x)
  (require 'turtles)


If you checked out the source from
`<https://github.com/szermatt/turtles>`_, you'll find the tests shown
in this tutorial in the file `test/turtles-example-test.el
<https://github.com/szermatt/turtles/blob/master/test/turtles-examples-test.el>`_
and you can run them interactively or in batch mode using :code:`eldev
test`.

.. _tut_hello_world:

Screen Grabbing
---------------

To get started, let's create a test that creates a buffer, renders it
and check the result:

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


The first call in the test is :ref:`(turtles-ert-test)<ert>`. This
function creates a secondary Emacs instances, then runs the rest of
the test within that instance.

What Turtles calls instance is a separate Emacs process that
:code:`turtles-ert-test` started within a terminal window.

Running within a secondary instance is only useful because it is
needed by :ref:`(turtles-with-grab-buffer) <grab>`. This macro
displays its containing buffer in a window, grabs the content of that
window and puts than into an ERT test buffer.

The body of :code:`turtles-with-grab-buffer` runs within that grabbed
buffer, just like the body of :code:`ert-with-test-buffer`. Its content
can be modified and checked with the usual tools.

Obviously, the window that was grabbed didn't have just two lines and
was larger than just the two words that appear here. What was really
grabbed contained spaces and newlines that
:code:`turtles-with-grabbed` trimmed automatically to make it easier
to test.

Try passing the option :code:`:trim t` and running the test with
:code:`ert-run-tests-interactively`:

.. code-block:: elisp

      (turtles-with-grab-buffer (:trim t)
        (should (equal "hello, world!"
                       (buffer-string))))))

You'll then see something like the following:

.. code-block::

  F turtles-examples-hello-world
      Buffer: *Test buffer (turtles-examples-hello-world)*
      Buffer: *Test buffer (turtles-examples-hello-world): grab*
      (ert-test-failed
       ((should
         (equal "hello, world!"
                (buffer-string)))
        :form
        (equal "hello, world!"
               #("hello, world!\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" 0 13
                 (face
                  (...))
                 13 35
                 (face default)))
        :value nil :explanation
        (arrays-of-different-length 13 35 "hello, world!"
                                    #("hello, world!\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" 0 13
                                      (face
                                       (...))
                                      13 35
                                      (face default))
                                    first-mismatch-at 13)))

As you can see above, the window that was grabbed had a bit more than
20 lines. This corresponds to a single window within a 80x24, the size
of the default :ref:`instance <instances>`.

The ERT test buffers listed above

.. code-block::

      Buffer: *Test buffer (turtles-examples-hello-world)*
      Buffer: *Test buffer (turtles-examples-hello-world): grab*

are part of that instance. If you click on either one of these, you'll
be offered a choice of different ways of seeing these buffers. The
most convenient one, if you're running in a windowing environment, is
to ask the instance to create a new frame to show the buffer.

:code:`turtles-with-grab-buffer` doesn't just grab the window content,
of course, but actually the whole frame, then strips out everything
that's outside the window. To better understand what this means, add
the option :code:`frame t`, as shown below, and run the tests again:

.. code-block:: elisp

      (turtles-with-grab-buffer (:frame t)
        (should (equal "hello, world!"
                       (buffer-string))))))

They will fail, and in the error message, or the buffers listed there,
you'll see the entire Emacs frame that was grabbed, including the mode
line and message area.

Have a look at the :ref:`turtles-with-grab-buffer reference <grab>` to
see how you can grab other sections of the screen.

.. _tut_minibuffer:

Minibuffer
----------

This second example illustrates the use of
:ref:`(turtles-with-minibuffer) <minibuffer>` running
:code:`completing-read`:

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

          (turtles-input-keys "Ch TAB")
          (turtles-with-grab-buffer (:name "completion" :point "<>")
            (should (equal "Choose: Choice <>" (buffer-string))))

          (turtles-input-keys "B")))))


:code:`turtles-with-minibuffer` takes as argument two separate sections, as shown below:

.. code-block:: elisp

  (turtles-with-minibuffer
      READ
    BODY)


The READ section is a single sexp that calls a function that runs on
the minibuffer or within a recursive-edit. When this function returns,
:code:`turtles-with-minibuffer` ends and returns the result of
evaluating READ.

The example above doesn't care about what READ evaluates to, because
it checks the retrun value of :code:`completing-read` directly within
that section.

The BODY section is a series of sexp that is executed interactively
*while the READ section runs*. This isn't multi-threading;
:code:`turtles-with-minibuffer` waits for the READ sections to call
:code:`recursive-edit`, usually indirectly through
:code:`read-from-minibuffer`, and runs BODY within that interactive
session.

At the end of BODY, the minibuffer is closed, if needed, and control
returns to READ, which checks the result of running BODY.

Within that example BODY first checks the minibuffer content with:

.. code-block:: elisp

          (turtles-with-grab-buffer (:name "initial prompt" :point "<>")
            (should (equal "Choose: <>" (buffer-string))))

The argument :point tells :code:`turtles-with-grab-buffer` to
highlight the position of the cursor with "<>". You can also just
check that manually; it's just convenient to see the content and the
position of the point in the same string.

This test interacts with :code:`completing-read` by simulating the
user typing some text and pressing :kbd:`TAB`. It uses
:ref:`turtles-input-keys <input>` for that, which simulates the user
typing some keys.

The test could have called the command :kbd:`TAB` corresponds to directly:

.. code-block:: elisp

        (turtles-input-keys "Ch")
        (minibuffer-complete)
        (turtles-with-grab-buffer (:name "completion" :point "<>")
          (should (equal "Choose: Choice <>" (buffer-string))))

Calling interactive commands in such a way in a test is usually
clearer than going through key bindings, and in most cases, it works well.

Some commands that rely on the specific environment provided by the
command loop won't work if called directly.
:ref:`turtles-input-command <input>` can help with such commands,
though it's just overkill here:

.. code-block:: elisp

        (turtles-input-keys "Ch")
        (turtles-input-command #'minibuffer-complete)
        (turtles-with-grab-buffer (:name "completion" :point "<>")
          (should (equal "Choose: Choice <>" (buffer-string))))


.. _tut_isearch:

Faces and Point
---------------

This last example tests isearch. While not a minibuffer-based command,
isearch still works with :code:`turtles-with-minibuffer`.

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


The interesting bit here is:

.. code-block:: elisp

          (turtles-with-grab-buffer (:buf testbuf :name "match 1" :faces '((isearch . "[]")))
            (should (equal "[Baa], baa, black sheep, have you any wool?"
                           (buffer-string))))

This test is used to check which part of the buffer isearch
highlighted.

The argument :faces tells :code:`turtles-with-grab-buffer` to grab a
small set of faces and make them available in the buffer as the text
property 'face.

This example additionally asks :code:`turtles-with-grab-buffer` to
detect portions of the buffer with such a face and surround them with
brackets, to make it more convenient to test.

Faces aren't available in a terminal, of course. Turtles uses colors
to highlight the faces it's interested in, then processes the grabbed
data to recognize the faces it wants from these colors it has
assigned.
