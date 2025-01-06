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

Version 1.0.0 was just released. Please depend on that tag.

Since this is a library, versioning matters: This project follows
[Semantic Versioning 2.0.0](https://semver.org/), that is, versions
follow the format MAJOR.MINOR.PATCH with:

- MAJOR incremented for incompatible API changes
- MINOR when functionality is added that is backward-compatible
- PATCH for bugfixes

To avoid surprises, you should aim to depend on a specific, tagged
version and only increase that version when you need to.

## Installation

Install Turtles:

- on a recent version of Emacs (29 or later), from the
  github repository by doing `M-x package-vc-install https://github.com/szermatt/turtles`

- using [eldev](https://github.com/emacs-eldev/eldev) to run tests in
  batch mode:

  ```elisp

    (eldev-add-extra-dependencies 'test 'turtles)
    (eldev-use-vc-repository 'turtles :github "szermatt/turtles" :commit "1.0.0")
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

(turtles-ert-deftest turtles-test-hello-world ()
  ;; The body of turtles-ert-deftest runs inside a
  ;; secondary Emacs instance.

  (ert-with-test-buffer ()
    (insert "hello, ")           ;; Fill in the buffer
    (insert (propertize "the " 'invisible t))
    (insert "world!\n")

    (turtles-with-grab-buffer () ;; Grab the current buffer content
      ;; Check the buffer content that was displayed
      (should (equal "hello, world!"
                     (buffer-string))))))

```

## Something is wrong!

Please check the [doc](https://turtles.readthedocs.io/en/latest/)
and, if that doesn't help, take the time to [file a bug report](https://turtles.readthedocs.io/en/latest/contrib.html#reporting-issues).

## Contributing

See the [Contributing](https://turtles.readthedocs.io/en/latest/contrib.html)
section of the documentation.



