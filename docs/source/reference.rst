.. _ref:

Reference
=========

.. _ert:

ERT Integration
---------------

.. index::
   pair: function; turtles-ert-deftest
   pair: function; turtles-upstream
   pair: function; turtles-this-instance

Turtles runs ERT tests is a secondary Emacs instance, which is started
and piloted by Turtles. This is what allows everything else in this
section to work.

(turtles-ert-deftest name (&key instance timeout) body) : macro

      This macro define an ERT test with the given name that runs its
      content inside of a secondary Emacs instance. 

      The ID of the instance to connect can be passed to the key
      argument :instance. That ID defaults to ``default``, an
      instance with a 80x24 terminal. (:ref:`instances`)

      A :timeout value, in seconds, can be passed to tell Turtles how
      long to wait for an answer from the secondary instance. Increase
      this value if you're getting timeout errors.

      The body of the macro is just like the body of a
      ``ert-deftest``, that is, it can contain:

      - an optional docstring
      - :tags and :expected-result key arguments
      - the test body possibly containing calls to (should), (should-not),
        (skip-when) or (skip-unless), defined by ``ert-deftest``.

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
      ``turtles-io-conn`` struct. (:ref:`rpc`)

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
grabbed and fully-processed: ``turtles-with-grab-buffer`` and
``turtles-to-string``, described below.

All functions that grab the terminal frame must be called from within
a secondary instance, that is, inside a ``(turtles-ert-deftest)``.

When grabbing the terminal frame, the content of the current buffer is
replaced with a copy of the terminal data, with the point set at the
position of the cursor.

In that buffer, color and similar text attributes are available as a
``'face`` text property. Starting with Emacs 29.1, the terminal
supports 24bit colors, but older Emacs versions must do with 16 or
even 8 colors. This usually doesn't matter as it's more convenient to
check for faces rather than colors, see the :faces key argument below.

(turtles-with-grab-buffer (&key ...) &rest body) : macro
      This macro creates an ERT test buffer, grabs the specified
      portion of the frame, post-processes it, then evaluates BODY.

      It then runs BODY with the ERT test buffer as current buffer.
      BODY usually checks the buffer content with ``should`` and
      ``should-not``. At the end of BODY, the buffer is killed unless
      the test failed.

      Key arguments:

        *:name NAME* specifies the buffer name, "grab" by default. It is
        forwarded to ``ert-with-test-buffer``.

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
        to test using just ``(equals ... (buffer-string))``.

        FACE-LIST-OR-ALIST is a list of either:

        - the face to grab, a symbol
        - ( face pair ) with pair being a string that can be split into
          opening and closing strings, for example "()", "[]" or even "<<>>".
        - ( face opening closing ) opening being
          opening and closing strings, for example "face1:(" ")"

        :trim nil tells the macro not to remove trailing whitespaces
        and newlines.

(turtles-to-string) : macro
      This macro works just like ``turtle-with-grab-buffer`` and
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
functionality. Usually, that's all you need. The macros calls the
functions below, which are then only useful if you choose to use
neither ``turtles-with-grab-buffer`` nor ``turtles-to-string``.


(turtles-grab-frame &optional win grab-faces) : function
      This puts the content of the terminal frame into the current
      buffer and sets the point at the position where the cursor is.

      WIN is a window that must be selected while grabbing.

      GRAB-FACES is a list of face symbols to grab. See the
      description of the :faces argument on
      ``turtles-with-grab-buffer``. (:ref:`grab`)

(turtles-grab-window win &optional grab-faces margin) : function
      This function puts the content of WIN into the current buffer
      and puts the point at the position where the cursor is.

      GRAB-FACES is a list of face symbols to grab. See the
      description of the :faces argument on
      ``turtles-with-grab-buffer``. (:ref:`grab`)

      If MARGIN is non-nil, grab not only the body of the window, but
      also the left and right margins.

(turtles-grab-buffer buf &optional grab-faces margins) : function
      This function grabs BUF into the current buffer.

      If BUF is shown on a window already, that window is the one
      that's grabbed. Otherwise, BUF is installed in the root window
      of the frame before grabbing.

      This function otherwise behaves as ``turtles-grab-window``.
      See that function for details.

(turtles-grab-mode-line win-or-buf &optional grab-faces) : function
      This function grabs the mode line of the specified WIN-OR-BUF, a
      window or buffer.

      GRAB-FACES is a list of face symbols to grab. See the
      description of the :faces argument on
      ``turtles-with-grab-buffer`` (:ref:`grab`)

(turtles-grab-header-line win-or-buf &optional grab-faces) : function
      This function grabs the header line of the specified WIN-OR-BUF,
      a window or buffer.

      GRAB-FACES is a list of face symbols to grab. See the
      description of the :faces argument on
      ``turtles-with-grab-buffer``. (:ref:`grab`)

(turtles-mark-text-with-faces alist) : function
      This function marks faces in the current buffer, as does the
      :face argument of ``turtles-with-grab-buffer``. It detects the
      regions with a specific face in the current buffer and surrounds
      them with an opening and a closing string, provided in the
      alist.

      ALIST is a list of, either:

      - ``( face pair )`` with pair being a string that can be split into
        opening and closing strings, for example "()", "[]" or even
        "<<>>".

      - ``( face opening closing )`` with separate opening and closing
        strings, for example "face1:(" ")"

      Note that for this function to work, the faces must have been
      grabbed by one of the grab functions.

(turtles-mark-text-with-face face opening-or-pair &optional closing) : function
      This is a shortcut for ``turtles-mark-text-with-faces`` for
      marking a single face in the current buffer.

      FACE is the symbol of the face to mark, OPENING-OR-PAIR is
      either the opening string, or a string that can be split into
      opening and closing, such as "()", CLOSING is the closing
      string.

(turtles-mark-point STR) : function
      This function just calls (insert STR).

(turtles-trim-buffer) : function
      This function deletes trailing whitespaces on all lines and
      trailing newlines at the end of the current buffer.

.. _minibuffer:

Minibuffer
----------

.. index::
    pair: function; turtles-with-minibuffer


(turtles-with-minibuffer READ &rest BODY) : macro
    This macro tests minibuffer or recursive-edit interactions.
    It is meant to be called from within a secondary instance,
    that is, inside of a ``(turtles-ert-deftest)``.

    The first sexp within that macro, the READ section, calls a
    function that opens the minibuffer or a recursive-edit and waits
    for user interactions. When this function returns, the macro ends
    and returns whatever READ evaluates to.

    The first sexp within that macro, the READ section, calls a
    function that opens the minibuffer or a recursive-edit and waits
    for user interactions. When this function returns, the macro ends
    and returns whatever READ evaluates to.

    The rest of the sexp within the macro, the BODY section, are
    executed *while the READ section runs*. This isn't
    multi-threading, as ``turtles-with-minibuffer`` waits for the READ
    sections to call ``recursive-edit``, usually indirectly through
    ``read-from-minibuffer``, and then BODY within that interactive
    session.

    BODY is usually a mix of:

    - calls to ``turtles-with-grab-buffer`` to test the content of
      the minibuffer or any other window.

    - keys passed to the minibuffer, with (execute-kbd-macro) or :keys (see
      below for :keys).

    - commands that manipulate the minibuffer, called directly, using
      (ert-simulate-command) or using :command (see below for :command).

    At the end of BODY, the minibuffer is closed, if needed, and
    control returns to READ, which checks the result of running BODY.

    Special forms are available within BODY to simulate the user inputing
    events using the command loop. In contrast to ``execute-kbd-macro``,
    ``ert-simulate-commands`` and ``ert-simulate-keys``, these
    function use the real event loop, triggered by real, external events
    (terminal keys). This isn't as simulation.

    You can't use these special form except directly in BODY. The
    following won't work, for example: ``(if cond :keys "abc")``

    :keys keys
        This expression provides KEYS as user input to the minibuffer.

        KEYS is in the same format as passed to ``kbd``.

        Prefer ``(execute-kbd-macro)``, when it works.

    :events events
        This expression provides a vector of events as the user input
        to the minibuffer.

        This is more general than the previous function as the events
        can be any kind of UI events.

        Prefer ``(execute-kbd-macro)``, when it works.

    :command command
        This expression runs the given interactive command in the event
        loop, triggered by a key stroke.

        Prefer calling the command directly or through
        ``(ert-simulate-command)``, when it works.

    :command-with-keybinding keybinding command
        This expression works as above, but makes sure that the command
        will find in ``(this-command-keys)``, if it asks.

    Usage examples: :ref:`tut_minibuffer` and :ref:`tut_isearch`

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

Turtles starts secondary Emacs instances from the main process. These
instances run the same version of Emacs with the same
``load-path``, in vanilla mode, without configuration.

The secondary Emacs instances are run within a hidden
``term-mode`` buffer. Such buffers are called "
*turtles-term-<instance-name>*" (note the space). You may switch to
that buffer to interact directly with the Emacs instance. To see
colors, rename it, as Emacs doesn't bother processing 'font-lock-face
in hidden buffers.

While secondary instances can be interacted with from that buffer, it
is awkward, as the two Emacs instances use the same keybindings. You
might be happier calling ``turtles-new-frame-in-instance`` (:ref:`visit`)
if you're running in a windowing environment, or otherwise
``turtles-instance-eval``. (:ref:`instances`)

The main Emacs process communicates with the secondary instances using
socket communication described in the next section :ref:`rpc`. On
startup, the instances connect to the server, and, from then on,
communicate with the server through RPCs.

There can be multiple secondary instances, identified by a symbol,
their ID. Instances with different ids have different characteristics,
defined by ``turtles-definstance``, described below. Turtles
defines one shared instance in a 80x25 terminal whose ID is 'default.
This is the instance used by ERT tests unless specified otherwise.

Secondary instances can be started and stopped independently using
``turtles-start-instance`` and ``turtles-stop-instance``, and
communicated with using ``turtles-instance-eval``.

During development, the versions of elisp libraries might get out of
sync between the main Emacs process and secondary instances. In such a
case, the simplest thing to do is to restart all live instances with
``turtles-restart``.

(turtles-start-server) : function
    This function creates a ``turtles-io-server`` (:ref:`rpc`)
    for instances to connect to. It doesn't start any instances.

    Calling this function is usually not necessary, as the server is
    started automatically before starting the first instance.

(turtles-shutdown) : command
    This function stops the current ``turtles-io-server``
    (:ref:`rpc`) if it is running, as well as all instances connected
    to it.

(turtles-restart) : command
    This function shuts down the current server, then restarts any
    live instances.

(cl-defstruct turtles-instance id doc conn width height forward setup term-buf): struct
    This structure stores information about instances.

    Use ``turtles-definstance`` to create and register instances
    of this struct and call ``turtles-get-instance`` to find an
    instance by its ID.

    ID is the instance ID.

    CONN is a ``turtles-io-conn`` (:ref:`rpc`) to use to communicate
    with the instance.

    WIDTH, WEIGHT, FORWARD and SETUP are as passed to
    ``turtles-definstance``. See below for details.

    TERM-BUF is the term-mode buffer within which the instance is
    running, if it is running.

(turtles-definstance id (&key ...) doc setup) : macro
    Define a new instance with the given ID.

    Turtles defines a shared instance with ID ``default``. This is
    the instance used by :ref:`turtle-ert-test <ert>` unless a
    specific one is given. The default instance starts a 80x24
    terminal with no setup.

    Define your own custom instance whenever you need a different
    screen size, setup or to forward the value of variables at
    startup.

    Make sure you set at least a short documentation in DOC. This
    documentation is displayed in the prompt of
    ``turtles-start-instance``, ``turtles-stop-instance`` and
    in the message issued when an instance is started.

    The code in SETUP is executed before every ERT test. This is a
    convenient place to put Emacs instance setup that you want to
    remain constant across tests.

    This macro takes the following key arguments:

    :width WIDTH and :height HEIGHT to set the dimensions of the
    terminal.

    :forward SYMBOL-LIST provides a list of variable symbols whose
    value should be copied to the instance at launch. This is useful
    if you have variables whose value influence the tests that you
    want to remain consistent between the main Emacs process and the
    secondary instance.

    Example:

    .. code-block:: elisp

      (turtles-definstance my-instance (:width 132 :height 43)
        "Emacs instance within a larger terminal.")


(turtles-get-instance inst-or-id) : function
    This function returns a ``turtles-instance``. Given an ID, it
    returns the instance with that ID, or nil if it cannot be found.

    Given a ``turtles-instance``, it returns that instance. This
    is useful to setup functions that take either an ID or an
    instance. Such function just need to call
    ``turtles-get-instance`` at startup.

(turtles-instance-alist) : variable
    This alist maps ``turtles-instance`` IDs to their value.

    This alist is normally only filled by ``turtles-definstance``.

(turtles-instance-shortdoc inst-or-id) : function
    Return a short description for the given ``turtles-instance``
    or ID.

    The short description is built by taking the first line of the
    documentation set in ``turtles-definstance``.

(turtles-instance-live-p inst) : function
    Return non-nil if the given instance is live.

(turtles-instance-eval inst-or-id expr &key timeout) : function
    Evaluate EXPR on the given instance, identified by its ID or
    ``turtle-instance``.

    This function waits for the evaluation to finish and returns the
    result of that evaluation. If that evaluation is likely to take
    time, set TIMEOUT to a value longer than the default 10s.

    This function provides a convenient way to probe the internals of
    an Emacs instance from the comfort of the main Emacs process.

    For example, if you want to see what buffers are opened in the
    secondary emacs instance, you can run :kbd:`M-x eval-expression`
    and evaluate :code:`(turtles-instance-eval 'default
    '(buffer-list))`.

(turtles-start-instance inst-or-id) : command
    Start the given instance, unless it is already started.

    If called interactively, ask for the instance to start among the
    registered instances that aren't live yet.

(turtles-stop-instance inst-or-id) : command
    Stop the given instance, if it is running.

    If called interactively, ask for the instance to stop among the
    registered instances that are currently live.

(turtles-read-instance &optional prompt predicate) : function
    Ask the use to choose an instance among those for which PREDICATE
    evaluates to t.

    PROMPT is displayed in the minibuffer.

    PREDICATE takes a ``turtles-instance`` and should return
    non-nil to accept that instance.

(turtles-live-instances) : function
    Return the IDs of all live instances.

.. _visit:

Visiting Instance Buffers
-------------------------

When a ERT tests is run inside a secondary Emacs instance, buffers
referenced in the test result should be looked up in the instance that
ran the test, and not the main Emacs process.

Such remote processes can be found in the test result or backtrace as
``'(turtles-buffer :name "..." :instance id)``. To visit such a
buffer, call ``turtles-pop-to-buffer``

.. index::
    pair: function; turtles-new-frame-in-instance
    pair: function; turtles-pop-to-buffer
    pair: function; turtles-pop-to-buffer-embedded
    pair: function; turtles-pop-to-buffer-copy
    pair: function; turtles-pop-to-buffer-new-frame
    pair: function; turtles-pop-to-buffer-actions
    pair: function; turtles-pop-to-buffer-action-history


(turtles-new-frame-in-instance inst-or-id) : command
    When the main Emacs instance is run in a windowing environment,
    you can ask the secondary Emacs instance to open a new frame and
    inspect its state with this function.

    When called interactively, it lets the use choose an instance
    among those currently live.

(turtles-pop-to-buffer buffer) : function
    This function displays buffers of the form ``'(turtles-buffer :name "..." :instance id)``

    To do so, it looks in ``turtles-pop-to-buffer-actions`` for
    available actions and ask the user to choose one if there are more
    than one. To skip this step, make sure that there's only one
    action on that list.

(turtles-pop-to-buffer-embedded ...) : function
    This function displays a buffer from another instance in the
    terminal buffer of the main Emacs process. It is meant to be called
    by ``turtles-pop-buffer``.

(turtles-pop-to-buffer-copy ...) : function
    This function makes a copy of a buffer in another instance and
    displays it in the main Emacs process. It is meant to be called by
    ``turtles-pop-buffer``.

(turtles-pop-to-buffer-new-frame ...) : function
    This function tells the secondary instance owning the buffer to
    display to open a new frame showing that buffer. Only works if the
    main Emacs process is running in a windowing environment. It is
    meant to be called by ``turtles-pop-buffer``.

(turtles-pop-to-buffer-actions) : variable
    List of actions that ``turtles-pop-to-buffer`` should consider.

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

turtles-io defines a very simple communication protocol for Emacs
instances to communicate with each other, inspired from JSON-RPC. It
is used to allow the main Emacs process and the secondary instances to
communicate.

The protocol is based on a socket-based communication between the main
Emacs process, the server, and the secondary Emacs instances, the
client.

Each side communicate with the other by sending messages
separated by ``\n"""\n``. The messages are elisp expression of
the following form:

- a method call:

  .. code-block:: elisp

    (:id id :method method-name :params params)

  METHOD is the method name to call.

  ID is used to identify the response when it comes. If no ID is
  provided, the method is run, but no response is ever sent back. Such
  a method call without ID is called a notification.

  PARAMS is a lisp type defined by the method as its parameter. It
  might be nil or missing.

- a result:

  .. code-block:: elisp

    (:id id :result result)

  This is a response to a previous method call. ID echoes the ID that
  was passed to that call and RESULT is a lisp expression that the
  method returns. It might be nil, but it cannot be missing.

- an error:

  .. code-block:: elisp

    (:id id :error error)

  This is a response to a previous method call. ID echoes the ID that
  was passed to that call and RESULT should be a list expression of the same
  type as those captured by ``condition-case``. The CAR of that list is
  an error symbol and the CDR its argument. Note that different processes
  might not agree on the set of defined error symbols, so it is possible to
  receive an error whose CAR is not an error symbol.


The elisp expressions are serialized using ``prn1`` and read back
using ``read``. Many Emacs types cannot be serialized that way, so
Turtles defines placeholders for them:

  - buffers: (turtles-buffer :name NAME) or (turtles-buffer :live
    nil). Such placeholders can be opened from the main Emacs process
    with ``pop-to-buffer`` (:ref:`visit`)

  - window: (turtles-buffer :buffer BUFFER-NAME)

  - overlay: (turtles-overlay :from POS :to POS :buffer BUFFER-NAME)

  - marker: (turtles-marker :pos POS :buffer BUFFER-NAME)

  - frame: (turtles-frame :name TITLE)

  - anything else: (turtle-obj :type TYPE)

When running inside of a secondary Emacs instance, such placeholder
type are extended to include :instance ID to identify the source
instance.


(turtles-io-server socket &optional method-alist) : function
    Create a new server, listening to the given SOCKET file.

    METHOD-ALIST associates method ID to method handlers. A method
    handles takes 4 arguments: conn, id, method, params and should
    call one of ``turtles-io-send-result`` or
    ``turtles-io-send-error`` once it is finished.

    Return an instance of type ``turtles-io-server``.

(turtles-io-server-live-p server) : function
    Return non-nil if the given ``turtles-io-server`` instance is live.

(turtles-io-connect socket &optional method-alist) : function
    Connect to a server running at the given SOCKET file.

    METHOD-ALIST associates method ID to method handlers. A method
    handles takes 4 arguments: conn, id, method, params and should
    call one of ``turtles-io-send-result`` or
    ``turtles-io-send-error`` once it is finished.

    Return an instance of type ``turtles-io-conn``.

turtles-io-conn : struct
    This type represents a connection to some other Emacs instance.

(turtles-io-conn-live-p conn) : function
    Retrun non-nil if the given ``turtles-io-conn`` is live.

(turtles-io-unreadable-obj-props) : variable
    Properties to add to any placeholder generated for unreadable
    (unserializable) objects such as buffers.

(turtles-io-handle-method conn method params (&key timeout)) : function
    Call the given method on the connection with the given parameters.

    This function waits for the result and returns it. If the call
    returns an error, that error is sent as an signal.

(turtles-io-call-method-async conn method params handler) : function
    Alternative to the above method that doesn't wait for the result.
    The result or the error is instead passed to the given handler,
    which should take two arguments: result and error, only one of
    which is ever non-nil.

(turtles-io-notify conn method &optional params) : function
    Alternative to the above methods that doesn't expect a result.

(turtles-io-send-error conn id error) : function
    Send an error back to the called. Does nothing if the id is nil.

(turtles-io-send-result) : function
    Send a result back to the called. Does nothing if the id is nil.
