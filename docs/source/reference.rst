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

Turtles starts secondary Emacs instances from the main process. These
instances run the same version of Emacs with the same
:code:`load-path`, in vanilla mode, without configuration.

The secondary Emacs instances are run within a hidden
:code:`term-mode` buffer, which is grabbed upon request and sent to
the instances.

The main Emacs process communicates with the secondary instances using
socket communication described in the :ref:`next section <rpc>`. On
startup, the instances connect to the server, and, from then on,
communicate with the server through RPCs.

There can be multiple secondary instances, identified by a symbol,
their ID. Instances with different ids have different characteristics,
defined by :code:`turtles-definstance`, described below.

Turtles defines one shared instance in a 80x25 terminal whose ID is
'default. This is the instance used by ERT tests unless specified
otherwise.

Secondary instances can be started and stopped independently using
:code:`turtles-start-instance` and :code:`turtles-stop-instance`, and
communicated with using :code:`turtles-instance-eval`.

When developing, the versions of elisp libraries might get out of sync
between the main Emacs process and secondary instances. In such a
case, the simplest thing to do is to restart the instances with
:code:`turtles-restart`.

(turtles-start-server) : function
    This function starts a :ref:`turtles-io-server <rpc>` for instances
    to connect to. It doesn't start any instances.

    Calling this function is usually not necessary, the server is
    started automatically before starting the first instance.

(turtles-shutdown) : command
    This function shuts down the current :ref:`turtles-io-server
    <rpc>`, if any, as well as all instances connected to it.

(turtles-restart) : command
    This function shuts down the current server, then restarts any
    live instances.

(cl-defstruct turtles-instance id doc conn width height forward setup term-buf): struct
    This structure stores information about instances.

    Use :code:`turtles-definstance` to create and register instances
    of this struct and call :code:`turtles-get-instance` to find an
    instance by its ID.

    ID is the instance ID.

    CONN is the :ref:`turtles-io-conn <rpc>` to use to communicate
    with the instance.

    WIDTH, WEIGHT, FORWARD and SETUP are as passed to
    :code:`turtles-definstance`, see below for details.

    TERM-BUF is the term-mode buffer within which the instance is
    running, if it is running.

(turtles-definstance id (&key ...) doc setup) : macro
    Define a new instance with the given ID.

    Turtles defines a shared instance with ID :code:`default`. This is
    the instance used by :ref:`turtle-ert-test <ert>` unless a
    specific one is given. The default instance starts a 80x24
    terminal with no setup.

    Define your own custom instance whenever you need a different
    screen size, setup or to forward the value of variables at
    startup.

    Make sure you set at least a short documentation in DOC. This
    documentation is displayed in the prompt of
    :code:`turtles-start-instance`, :code:`turtles-stop-instance` and
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

(turtles-get-instance inst-or-id) : function
    This function returns a :code:`turtles-instance`. Given an ID, it
    returns the instance with that ID, or nil if it cannot be found.

    Given a :code:`turtles-instance`, it returns that instance. This
    is useful to setup functions that take either an ID or an
    instance. Such function just need to call
    :code:`turtles-get-instance` at startup.

(turtles-instance-alist) : variable
    This alist maps :code:`turtles-instance` IDs to their value.

    This alist is normally only filled by :code:`turtles-definstance`.

(turtles-instance-shortdoc inst-or-id) : function
    Return a short description for the given :code:`turtles-instance`
    or ID.

    The short description is built by taking the first line of the
    documentation set in :code:`turtles-definstance`.

(turtles-instance-live-p inst) : function
    Return non-nil if the given instance is live.

(turtles-instance-eval inst-or-id expr &key timeout) : function
    Evaluate EXPR on the given instance, identified by its ID or
    :code:`turtle-instance`.

    This function waits for the evaluation to finish and returns the
    result of that evaluation. If that evaluation is likely to take
    time, set TIMEOUT to a value longer than the default 10s.

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

    PREDICATE takes a :code:`turtles-instance` and should return
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
:code:`'(turtles-buffer :name "..." :instance id)`. To visit such a
buffer, call :code:`turtles-pop-to-buffer`

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
    This function displays buffers of the form :code:`'(turtles-buffer :name "..." :instance id)`

    To do so, it looks in :code:`turtles-pop-to-buffer-actions` for
    available actions and ask the user to choose one if there are more
    than one. To skip this step, make sure that there's only one
    action on that list.

(turtles-pop-to-buffer-embedded ...) : function
    This function displays a buffer from another instance in the
    terminal buffer of the main Emacs process. It is meant to be called
    by :code:`turtles-pop-buffer`.

(turtles-pop-to-buffer-copy ...) : function
    This function makes a copy of a buffer in another instance and
    displays it in the main Emacs process. It is meant to be called by
    :code:`turtles-pop-buffer`.

(turtles-pop-to-buffer-new-frame ...) : function
    This function tells the secondary instance owning the buffer to
    display to open a new frame showing that buffer. Only works if the
    main Emacs process is running in a windowing environment. It is
    meant to be called by :code:`turtles-pop-buffer`.

(turtles-pop-to-buffer-actions) : variable
    List of actions that :code:`turtles-pop-to-buffer` should consider.

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

Each side communicate with the other by sending elisp expressions
separated by :code:`\n"""\n`. Each elisp expression is a message,
which can be of the following types:

- a method call of the form:

  .. code-block:: elisp

    (:id id :method method-name :params params)

  METHOD is the method name to call.

  ID is used to identify the response when it comes. If no ID is
  provided, the method is run, but no response is ever sent back. Such
  a method call without ID is called a notification.

  PARAMS is a lisp type defined by the method as its parameter. It
  might be nil or missing.

- a result of the form:

  .. code-block:: elisp

    (:id id :result result)

  This is a response to a previous method call. ID echoes the ID that
  was passed to that call and RESULT is a lisp expression that the
  method returns. It might be nil, but it cannot be missing.

- an error of the form:

  .. code-block:: elisp

    (:id id :error error)

  This is a response to a previous method call. ID echoes the ID that
  was passed to that call and RESULT should be a list expression of the same
  type as those captured by :code:`condition-case`. The CAR of that list is
  an error symbol and the CDR its argument. Note that different processes
  might not agree on the set of defined error symbols, so it is possible to
  receive an error whose CAR is not an error symbol.


The elisp expressions are serialized using :code:`prn1` and read back
using :code:`read`. Many Emacs types cannot be serialized that way, so
Turtles defines placeholders for them:

  - buffers: (turtles-buffer :name NAME) or (turtles-buffer :live
    nil). Such placeholders can be opened from the main Emacs process
    with :ref:`pop-to-buffer <visit>`

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
    call one of :code:`turtles-io-send-result` or
    :code:`turtles-io-send-error` once it is finished.

    Return an instance of type :code:`turtles-io-server`.

(turtles-io-server-live-p server) : function
    Return non-nil if the given :code:`turtles-io-server` instance is live.

(turtles-io-connect socket &optional method-alist) : function
    Connect to a server running at the given SOCKET file.

    METHOD-ALIST associates method ID to method handlers. A method
    handles takes 4 arguments: conn, id, method, params and should
    call one of :code:`turtles-io-send-result` or
    :code:`turtles-io-send-error` once it is finished.

    Return an instance of type :code:`turtles-io-conn`.

turtles-io-conn : struct
    This type represents a connection to some other Emacs instance.

(turtles-io-conn-live-p conn) : function
    Retrun non-nil if the given :code:`turtles-io-conn` is live.

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
