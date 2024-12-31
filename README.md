# Turtles

[![test](https://github.com/szermatt/turtles/workflows/test/badge.svg)](https://github.com/szermatt/turtles/actions)
[![Documentation Status](https://readthedocs.org/projects/turtles/badge/?version=latest)](https://turtles.readthedocs.io/en/latest/?badge=latest)

This package help write ERT-based tests to that check how Emacs
renders buffers and windows. The ERT tests can be run interactively or
in batch mode.

It's especially suited to test:

- the effect of display, before-string, after-string text properties
- the effect of overlays
- text visibility
- status line
- colors changes
- complex minibuffer interactions

Read the full documentation on
[turtles-readthedocs.io](https://turtles.readthedocs.io/en/latest/index.html)
or in the info manual that comes with the package.

## Status

This project is getting close to stable, though API changes are still possible.

## Installation

Install Turtles:

- on a recent version of Emacs (29 or later), from the
  github repository by doing `M-x package-vc-install https://github.com/szermatt/turtles`

- using [eldev](https://github.com/emacs-eldev/eldev) to run tests in
  batch mode:

  ```elisp

    (eldev-add-extra-dependencies 'test 'turtles)
    (eldev-use-vc-repository 'turtles :github "szermatt/turtles")
  ```

Turtles requires Emacs 26.1 or later. Emacs 29.1 or later is recommended.

## How it works

Turtles starts another Emacs process inside of a term buffer, runs ERT
test in that instance and grab snapshot of the display whenever
instructed.

The result is that the frame, minibuffer, window content and status
line are available to tests as text with font-lock-face properties
specifying things like color.

Here's a quick example of a test that checks a text with an invisible
section.

For more details, see the
[Tutorial](https://turtles.readthedocs.io/en/latest/tutorial.html)

```elisp
(require 'turtles)

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
