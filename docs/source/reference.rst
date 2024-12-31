.. _ref:

Reference
=========

.. _ert:

ERT Integration
---------------

.. index::
   pair: function; turtles-ert-test
   pair: function; turtles-upstream
   pair: function; turtles-this-instance

Turtles runs ERT tests is a secondary Emacs instance, which is started
and piloted by Turtles. This is what allows everything else in this
section to work.

(turtles-ert-test &key instance timeout) : macro
      This macro marks the current test as a Turtles test. It connects
      to a secondary Emacs instance, starts it if necessary, then runs
      everything below (turtles-ert-test) in that secondary instance.

      The ID of the instance to connect can be passed to the key
      argument :instance. That ID defaults to :code:`default`, with a
      80x24 terminal. See the :ref:`instances` section for details on
      how to define new instances.

      A :timeout value, in seconds, can be passed to tell Turtles how
      long to wait for an answer from the secondary instance. Increase
      this value if you're getting timeout errors.

(turtles-this-instance) : function
      When run in a secondary Emacs instance, this function returns
      the instance ID. It returns nil when not called from an
      instance.

      Calling this function provides a convenient way of knowing
      whether the current code is running in the main Emacs process or
      a secondary instance started by Turtles.

(turtles-upstream) : function
      When called from a secondary Emacs instance, this function
      returns the connection to the main Emacs process, a
      :code:`turtles-io-conn` struct, described in section :ref:`rpc`.

      When called from the main Emacs process, this function returns
      nil.

.. _grab:

Screen Grab
-----------

.. index::
    pair: function; turtles-with-grab-buffer
    pair: function; turtles-to-string
    pair: function; turtles-mark-text-with-face
    pair: function; turtles-mark-text-with-faces
    pair: function; turtles-mark-point
    pair: function; turtles-trim-buffer
    pair: function; turtles-grab-frame
    pair: function; turtles-grab-buffer
    pair: function; turtles-grab-mode-line
    pair: function; turtles-grab-header-line
    pair: function; turtles-grab-window

Two macros are provided that fully control how the terminal frame is
grabbed and fully-processed: :code:`turtles-with-grab-buffer` and
:code:`turtles-to-string`:

(turtles-with-grab-buffer (&key ...) &rest body) : macro
      This macro creates an ERT test buffer, grab the specified
      portion of the frame, post-processes it, then evaluates BODY.

      BODY then usually checks the buffer content :code:`should`.

      Key arguments:

        *:name NAME* specifies the buffer name, "grab" by default. It is
        forwarded to :code:`ert-with-test-buffer`.

      Key arguments that control what to grab:

        By default, the macro grabs the current buffer. If the buffer
        is already shown in a window, it grabs that window, otherwise
        it shows the buffer in a single window and grabs that.

        :buf BUFFER-OR-NAME specifies another buffer to grab

        :win WINDOW specifies a window to grab. The window doesn't
        have to be selected. However it will be selected when
        grabbing.

        :mode-line WIN-OR-BUF grabs the mode-line of the specified window or buffer.

        :header-line WIN-OR-BUF grabs the header-line of the specified window or buffer.

        :minibuffer t grabs the minibuffer window.

        :margins t grabs the left and right margin. This only has an
        effect when grabbing a buffer or a window.

        :frame t grabs the whole frame.

      Key arguments that control how to post-process what is grabbed:

        :point STR marks the position of the cursor with STR.

        :faces FACE-LIST-OR-ALIST specifies a set of faces to grab. To
        do that, Turtles assigns specific color to each face, grabs the
        result, then detects faces in the gabbed data from colors. This
        means that color data isn't available when this option is used.

        The face data can be recovered in the grabbed buffer in the text
        property 'face.

        Additionally, it is possible to specify strings to use to mark
        regions of the buffer with a specific face, to make it easier
        to test using just (equals ... (buffer-string)).

        FACE-LIST-OR-ALIST is a list of either:

        - the face to grab, a symbol
        - ( face pair ) with pair being a string that can be split into
          opening and closing strings, for example "()", "[]" or even "<<>>".
        - ( face opening closing ) opening being
          opening and closing strings, for example "face1:(" ")"

        :trim nil tells the macro not to remove trailing whitespaces
        and newlines.

(turtles-to-string) : macro
      This macro works just like :code:`turtle-with-grab-buffer` and
      takes the same arguments, described above. The only difference
      is that instead of opening an ERT test buffer, this function
      returns the buffer content as a string.

      So, instead of:

      .. code-block:: elisp

        (turtles-with-grab-buffer (...)
          ...
          (should (equal "..." (buffer-string))))

      you'd write:

      .. code-block:: elisp

        (should (equal "..." (turtles-to-string ...)))

      This is shorter, but doesn't make the buffer available for
      inspection when the test fails.


The two macros above form the frontend of the Turtles grabbing
functionality. Usually, that's all you need. The functions below
provide the functionality offered by these frontends and are only
useful if you choose to use neither :code:`turtles-with-grab-buffer`


(turtles-grab-frame &optional win grab-faces) : function
      This puts the content of the terminal frame into the current
      buffer and sets the point at the position where the cursor is.

      WIN is a window that must be selected while grabbing.

      GRAB-FACES is a list of face symbols to grab. See the
      description of the :faces argument on
      :ref:`turtles-with-grab-buffer <grab>` for details.

(turtles-grab-window win &optional grab-faces margin) : function
      This function puts the content of WIN into the current buffer
      and puts the point at the position where the cursor is.

      GRAB-FACES is a list of face symbols to grab. See the
      description of the :faces argument on
      :ref:`turtles-with-grab-buffer <grab>` for details.

      If MARGIN is non-nil, grab not only the body of the window, but
      also the left and right margins.

(turtles-grab-buffer buf &optional grab-faces margins) : function
      This function grabs BUF into the current buffer.

      If BUF is shown on a window already, that window is the one
      that's grabbed. Otherwise, BUF is installed in the root window
      of the frame before grabbing.

      This function otherwise behaves as :code:`turtles-grab-window`.
      See that function for details.

(turtles-grab-mode-line win-or-buf &optional grab-faces) : function
      This function grabs the mode line of the specified WIN-OR-BUF, a
      window or buffer.

      GRAB-FACES is a list of face symbols to grab. See the
      description of the :faces argument on
      :ref:`turtles-with-grab-buffer <grab>` for details.

(turtles-grab-header-line win-or-buf &optional grab-faces) : function
      This function grabs the header line of the specified WIN-OR-BUF,
      a window or buffer.

      GRAB-FACES is a list of face symbols to grab. See the
      description of the :faces argument on
      :ref:`turtles-with-grab-buffer <grab>` for details.

(turtles-mark-text-with-faces alist) : function
      This function marks faces does the :face of
      :code:`turtles-with-grab-buffer`.

      It detects the regions with a specific face in the current
      buffer and marks them.

      It takes a list of, either:

      - ( face pair ) with pair being a string that can be split into
        opening and closing strings, for example "()", "[]" or even
        "<<>>".

      - ( face opening closing ) opening being
        opening and closing strings, for example "face1:(" ")"

      Note that for this function to work, the faces must have been
      grabbed by one of the grab functions.

(turtles-mark-text-with-face face opening-or-pair &optional closing) : function
      This is a shortcut for :code:`turtles-mark-text-with-faces` for
      marking a single face in the current buffer.

      FACE is the symbol of the face to mark, OPENING-OR-PAIR is
      either the opening string, or a string that can be split into
      opening and closing, such as "()", CLOSING is the closing
      string.

(turtles-mark-point STR) : function
      This function just calls (insert STR).

(turtles-trim-buffer) : function
      This function delete trailing whitespaces on all lines and
      trailing newlines at the end of the current buffer.

.. _minibuffer:

Minibuffer
----------

.. index::
    pair: function; turtles-with-minibuffer


(turtles-with-minibuffer READ &rest BODY) : macro
    This macro tests minibuffer or recursive-edit interactions.

    The READ section is a single sexp that calls a function that runs
    on the minibuffer or within a recursive-edit. When this function
    returns, the macro ends and returns the result of evaluating READ.

    The BODY section is a series of sexp that is executed
    interactively *while the READ section runs*. This isn't
    multi-threading; :code:`turtles-with-minibuffer` waits for the
    READ sections to call :code:`recursive-edit`, usually indirectly
    through :code:`read-from-minibuffer`, and runs BODY within that
    interactive session.

    BODY is usually a mix of:

    - calls to :code:`turtles-with-grab-buffer` to test the content of
      the minibuffer or any other window.

    - keys passed to the minibuffer, with :code:`turtles-input-keys`

    - commands that manipulate the minibuffer, either called directly
      or using :code:`turtles-input-command`

    At the end of BODY, the minibuffer is closed, if needed, and
    control returns to READ, which checks the result of running BODY.

    See the :ref:`tut_minibuffer` and :ref:`tut_isearch` sections of
    the tutorial for usage examples.

.. _input:

Input Events
------------

.. index::
    pair: function; turtles-input-keys
    pair: function; turtles-input-events
    pair: function; turtles-input-command

The following functions send events to the Emacs instance to be
processed using the normal event loop.

In contrast to :code:`execute-kbd-macro`,
:code:`ert-simulate-commands` and :code:`ert-simulate-keys`, these
function use the real event loop, triggered by real, external events
(terminal keys). This isn't as simulation. In most cases, the
difference doesn't matter, of course.

These functions all use :code:`recursive-edit` to make it possible to
handle events directly from ERT tests.

(turtles-input-keys keys) : function
    This function provides KEYS as user input to the current instance.

    KEYS is in the same format as passed to :code:`kbd`.

(turtles-input-events events) : function
    This function provides a vector of events as the user input to the
    current instance.

    This is more general than the previous function as the events can
    be any kind of UI events.

(turtles-input-command command &optional keybinding) : function
    This function runs the given interactive command in the event
    loop, triggered by a key stroke.

    If provided, keybinding is what what the command will find in
    :code:`(this-command-keys)`, if it asks.

    This is implemented using a transient map, so the key binding is
    only available for one call.

    Note that in the majority of cases, calling the command directly,
    outside of the event loop, works just fine in tests and is more
    convenient.

.. _instances:

Instance Management
-------------------

.. index::
    pair: function; turtles-start-server
    pair: function; turtles-shutdown
    pair: function; turtles-restart
    pair: struct; turtles-instance
    pair: function; turtles-definstance
    pair: function; turtles-get-instance
    pair: variable; turtles-instance-alist
    pair: function; turtles-instance-shortdoc
    pair: function; turtles-instance-live-p
    pair: function; turtles-instance-eval
    pair: function; turtles-start-instance
    pair: function; turtles-stop-instance
    pair: function; turtles-read-instance
    pair: variable; turtles-live-instances


:code:`turtles-start-server` : function

:code:`turtles-shutdown` : function

:code:`turtles-restart` : function

:code:`turtles-instance` : struct

:code:`turtles-definstance` : macro

:code:`turtles-get-instance` : function

:code:`turtles-instance-alist` : variable

:code:`turtles-instance-shortdoc` : function

:code:`turtles-instance-live-p` : function

:code:`turtles-instance-eval` : function

:code:`turtles-start-instance` : function

:code:`turtles-stop-instance` : function

:code:`turtles-read-instance` : function

:code:`turtles-live-instances` : variable

.. _visit:

Visiting Instances
------------------

.. index::
    pair: function; turtles-new-frame-in-instance
    pair: function; turtles-pop-to-buffer
    pair: function; turtles-pop-to-buffer-embedded
    pair: function; turtles-pop-to-buffer-copy
    pair: function; turtles-pop-to-buffer-new-frame
    pair: function; turtles-pop-to-buffer-actions
    pair: function; turtles-pop-to-buffer-action-history


:code:`turtles-new-frame-in-instance` : function

:code:`turtles-pop-to-buffer` : function

:code:`turtles-pop-to-buffer-embedded` : function

:code:`turtles-pop-to-buffer-copy` : function

:code:`turtles-pop-to-buffer-new-frame` : function

:code:`turtles-pop-to-buffer-actions` : function

:code:`turtles-pop-to-buffer-action-history` : function

.. _rpc:

RPC (turtles-io)
----------------

.. index::
    pair: function; turtles-io-server
    pair: struct; turtles-io-server
    pair: function; turtles-io-server-live-p
    pair: function; turtles-io-connect
    pair: struct; turtles-io-conn
    pair: function; turtles-io-conn-live-p
    pair: variable; turtles-io-unreadable-obj-props
    pair: function; turtles-io-handle-method
    pair: function; turtles-io-send-error
    pair: function; turtles-io-send-result
    pair: function; turtles-io-call-method
    pair: function; turtles-io-notify
    pair: function; turtles-io-call-method-async


:code:`turtles-io-server` : function

:code:`turtles-io-server` : struct

:code:`turtles-io-server-live-p` : function

:code:`turtles-io-connect` : function

:code:`turtles-io-conn` : struct

:code:`turtles-io-conn-live-p` : function

:code:`turtles-io-unreadable-obj-props` : variable

:code:`turtles-io-handle-method` : function

:code:`turtles-io-send-error` : function

:code:`turtles-io-send-result` : function

:code:`turtles-io-call-method` : function

:code:`turtles-io-notify` : function

:code:`turtles-io-call-method-async` : function
