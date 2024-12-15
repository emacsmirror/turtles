# It's turtles all the way down

This package help write ERT-based tests to that check how Emacs
renders buffers and windows. The ERT tests can be run interactively or
in batch mode.

It's especially suited to test:

- the effect of display, before-string, after-string text properties
- the effect of overlays
- text visibility
- status line
- colors changes

## Status

This project is still in early experimental phase. It is undocumented
and shouldn't be used by anyone.

> [!IMPORTANT]
> Its API is highly unstable at this time. Serious refactoring are
> planned. Please wait before the first release before depending on
> this package.

## Installation

Emacs 29.1 or later is required.

The easiest way to install this package is to do: `M-x
package-vc-install https://www.github.com/szermatt/turtles`

## How it works

Turtles starts another Emacs process inside of a term buffer, runs ERT
test in that instance and grab snapshot of the display whenever
instructed.

The result is that the frame, minibuffer, window content and status
line are available to tests as text with font-lock-face properties
specifying things like color.

Here's a quick example of a test that checks a text with an invisible
section:

```elisp
(ert-deftest turtles-test-hello-world ()
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
```
