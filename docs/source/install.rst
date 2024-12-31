.. _install:

Installation
============

Install Turtles:

- on a recent version of Emacs (29 or later), from the
  github repository by doing :kbd:`M-x package-vc-install
  https://github.com/szermatt/turtles`

- from source, using an alternative package managers, such as `straight
  <https://github.com/radian-software/straight.el>`_, shown here:

  .. code-block:: elisp

    (use-package turtles
      :straight (:type git :repo "https://github.com/szermatt/turtles.git"))

- the old-fashioned way, copying the .el files from into your
  :file:`.emacs.d` directory.

- using `eldev <https://github.com/emacs-eldev/eldev>`_ to run tests in batch mode:

  .. code-block:: elisp

    (eldev-add-extra-dependencies 'test 'turtles)
    (eldev-use-vc-repository 'turtles :github "szermatt/turtles")

Turtles requires Emacs 26.1 or later. Emacs 29.1 or later is recommended.
