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

- from `MELPA <https://melpa.org/#/getting-started>`_, by typing `M-x
  package-install turtles`.

  Note that there's no practical difference between the stable and
  unstable MELPA package, as development happens on a branch. Both
  stable and unstable include the latest release of the code, unstable
  may contain a more recent version of the .info doc.

- the old-fashioned way, copying the .el files from into your
  :file:`.emacs.d` directory.

- using `eldev <https://github.com/emacs-eldev/eldev>`_ to run tests in batch mode:

  .. code-block:: elisp

    (eldev-add-extra-dependencies 'test 'turtles)
    (eldev-use-vc-repository 'turtles :github "szermatt/turtles" :commit "2.0.1")

This latest way is very much recommended as it allows specifying a version. To
avoid surprises, you should aim to depend on a specific, tagged
version and only increase that version when you need to.

This project follows `Semantic Versioning 2.0.0
<https://semver.org/>`_ that is, versions follow the format
MAJOR.MINOR.PATCH with:

- MAJOR incremented for incompatible API changes
- MINOR incremented when when functionality is added that is backward-compatible
- PATCH for bugfixes

Versions with "snapshot" appended, such as "1.3.1snapshot" are
development version and cannot be depended on. Please always work with
tagged versions.

Turtles requires Emacs 26.1 or later. Emacs 29.1 or later is recommended.
