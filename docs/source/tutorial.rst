.. _tut:

Tutorial
========

Turtles is a collection of tools for writing ERT tests that look at
the terminal showing an interactive Emacs instance from within that
instance. That is, you can setup buffer content and windows and check
how it looks like to a user running Emacs in a terminal.

Such ERT tests can run in batch mode as well an in interactive mode,
together with normal ERT tests.

To make that work, Turtles starts a secondary Emacs instance from
within a terminal buffer. Tests run in the secondary Emacs instance
and their result communicated to the primary Emacs instance. Whenever
needed, the primary instance grab the screen and provides the result,
that is, a terminal screen with colors and cursor position, to the
secondary instance.

To get started, install turtles (:ref:`install`), then create a new
test file with:

.. code-block:: elisp

  ;; -*- lexical-binding: t -*-

  (require 'ert)
  (require 'ert-x)
  (require 'turtles)


If you checked out the source from
`<https://github.com/szermatt/turtles>`_, you'll find the tests shown
in this tutorial in the file `test/turtles-example-test.el
<https://github.com/szermatt/turtles/blob/master/test/turtles-examples-test.el>`_.

.. _tut_hello_world:

Screen Grabbing with Hello World
--------------------------------

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


The first call in the test is ``(turtles-ert-test)``. (:ref:`ert`)
This function creates a secondary Emacs instances, then runs the rest
of the test within that instance. What Turtles calls instance is a
separate Emacs process that ``turtles-ert-test`` started within a
terminal window.

Running within a secondary instance is necessary because it is needed
by ``(turtles-with-grab-buffer)``. (:ref:`grab`) This macro displays
its containing buffer in a window, grabs the content of that window
and puts than into an ERT test buffer.

The body of ``turtles-with-grab-buffer`` runs that grabbed buffer as
current buffer, then kills the buffer at the end, unless the test
failed, just like the body of ``ert-with-test-buffer``. The content of
the buffer can be modified and checked with the usual tools.

In reality, the window that was grabbed didn't have just two lines and
was larger than just the two words that appear here. What was really
grabbed contained spaces and newlines that ``turtles-with-grabbed``
trimmed automatically to make it easier to test.

Try passing the option ``:trim t`` and re-running the test with
`M-x ert-run-tests-interactively`:

.. code-block:: elisp

      (turtles-with-grab-buffer (:trim t)
        (should (equal "hello, world!"
                       (buffer-string))))))

You'll get something like the following in the ``*ert*`` buffer::

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
20 lines. This corresponds to a single window within a 80x24 terminal,
the terminal dimensions of the default instance. (:ref:`instances`)

The ERT test buffers listed above::

      Buffer: *Test buffer (turtles-examples-hello-world)*
      Buffer: *Test buffer (turtles-examples-hello-world): grab*

are part of that instance. If you click on either one of these, you'll
be offered a choice of different ways of seeing these buffers. The
most convenient one, if you're running in a windowing environment, is
to ask the instance to create a new frame to show the buffer.

``turtles-with-grab-buffer`` doesn't just grab the window content, but
actually the whole frame, then strips out everything that's outside
the window. To better understand what this means, add the option
``:frame t``, as shown below, and run the tests again:

.. code-block:: elisp

      (turtles-with-grab-buffer (:frame t)
        (should (equal "hello, world!"
                       (buffer-string))))))

Running the above with ERT will fail, and in the error message and the
buffers listed there, you'll see the entire Emacs frame that was
grabbed, including the mode line and message area.

``turtles-with-grab-buffers`` (:ref:`grab`) supports different keyword
arguments that let you choose a section of the screen to grab and
post-process it.

.. _tut_minibuffer:

Minibuffer with completing-read
-------------------------------

This second example illustrates the use of
``(turtles-with-minibuffer)`` (:ref:`minibuffer`) running
``completing-read``:

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

          (execute-kbd-macro "Ch")
          (minibuffer-complete)
          (turtles-with-grab-buffer (:name "completion" :point "<>")
            (should (equal "Choose: Choice <>" (buffer-string))))

          (execute-kbd-macro "B")))))


``turtles-with-minibuffer`` takes as argument two separate sections, shown below:

.. code-block:: elisp

  (turtles-with-minibuffer
      READ
    BODY)


The READ section is a single sexp that calls a function that runs on
the minibuffer or within a recursive-edit. When this function returns,
``turtles-with-minibuffer`` ends and returns the result of
evaluating READ.

The example above doesn't care about what READ evaluates to, because
it checks the retrun value of ``completing-read`` directly within
that section.

The BODY section is a series of sexp that is executed interactively
*while the READ section runs*. This isn't multi-threading, as
``turtles-with-minibuffer`` waits for the READ sections to call
``recursive-edit``, usually indirectly through
``read-from-minibuffer``, and runs BODY within that interactive
session.

At the end of BODY, the minibuffer is closed, if needed, and control
returns to READ, which checks the result of running BODY.

Within that example BODY first checks the minibuffer content with:

.. code-block:: elisp

          (turtles-with-grab-buffer (:name "initial prompt" :point "<>")
            (should (equal "Choose: <>" (buffer-string))))

The argument :point tells ``turtles-with-grab-buffer`` to
highlight the position of the cursor with "<>". You can also check
that manually; it's just convenient to see the content and the
position of the point in the same string.

This test interacts with ``completing-read`` by simulating the
user typing some text and pressing :kbd:`TAB`.

The test could have directly called the command :kbd:`TAB` is bound
to:

.. code-block:: elisp

        (execute-kbd-macro "Ch")
        (minibuffer-complete)
        (turtles-with-grab-buffer (:name "completion" :point "<>")
          (should (equal "Choose: Choice <>" (buffer-string))))

Calling interactive commands in such a way in a test is usually
clearer than going through key bindings, and, in most cases, it works
well.

However, some commands that rely on the specific environment provided
by the command loop don't like being called directly or even through
``execute-kbd-macro``. :keys and :command (:ref:`minibuffer`) can help
in such tricky situations. Though it would be overkill here, you could
write:

.. code-block:: elisp

        :keys "Ch"
        :command #'minibuffer-complete
        (turtles-with-grab-buffer (:name "completion" :point "<>")
          (should (equal "Choose: Choice <>" (buffer-string))))


.. _tut_isearch:

Faces with Isearch
------------------

This last example tests isearch. While not a minibuffer-based command,
isearch still works with ``turtles-with-minibuffer``.

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
            (isearch-forward)

          :keys "baa"
          (turtles-with-grab-buffer (:minibuffer t)
            (should (equal "I-search: baa" (buffer-string))))
          (turtles-with-grab-buffer (:buf testbuf :name "match 1" :faces '((isearch "[]")))
            (should (equal "[Baa], baa, black sheep, have you any wool?"
                           (buffer-string))))

          :keys "\C-s"
          (turtles-with-grab-buffer (:buf testbuf :name "match 2" :faces '((isearch "[]")))
            (should (equal "Baa, [baa], black sheep, have you any wool?"
                           (buffer-string))))

          (isearch-done))

        (turtles-with-grab-buffer (:name "final position" :point "<>")
          (should (equal "Baa, baa<>, black sheep, have you any wool?"
                         (buffer-string)))))))


The interesting bit here is:

.. code-block:: elisp

          (turtles-with-grab-buffer (:buf testbuf :name "match 1" :faces '((isearch "[]")))
            (should (equal "[Baa], baa, black sheep, have you any wool?"
                           (buffer-string))))

The above checks which part of the buffer isearch highlighted. The
argument :faces tells ``turtles-with-grab-buffer`` to grab a small set
of faces and make them available in the buffer as the text property
'face.

This example additionally provides "[]", which tells
``turtles-with-grab-buffer`` to mark portions of the buffer that have
such a face with brackets. This way, we don't need to check text
properties in the test.

Faces aren't really available when grabbing a terminal screen. To make
this work, Turtles uses colors to highlight the faces it's interested
in, then recognize the faces it wants in the grabbed data from these
colors it has assigned.
