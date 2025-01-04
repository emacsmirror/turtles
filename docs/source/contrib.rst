Contributing
============

.. _reporting:

Reporting issues
----------------

At this time, the most useful thing you can do to help is and useful
bug reports to the `Issue Tracker`_

In your report, please discuss what you wanted to happen as well as
what happened. Also, please include enough information to reproduce
the issue.

Please include at least the version of turtles you're using and the version of Emacs you're running, taken, for example, from :kbd:`M-x about-emacs`.

Suggesting features
-------------------

Please add feature suggestions to the `Issue Tracker`_ or start a `discussion`_.

Asking questions
----------------

Start a `discussion`_ with your question.

Code contributions
------------------

To contribute code to the project, open a `Pull Request`_ targeted to
the ``dev`` branch.

Before you do that, make sure the any new features is covered by tests
and that the tests pass.

To run the tests, install and setup `eldev`_, then run :command:`eldev
test`.

Tests can also be run from inside of Emacs, using `M-x
ert-run-tests-interactively` but when you do so, be aware that there
might be unexpected interaction with your Emacs configurations. The
tests passing reliably when run using :command:`eldev test` is what
matters.

Please also make sure your commit message follows `Conventional
Commits 1.0.0 <https://www.conventionalcommits.org/en/v1.0.0/>`_, in
short, the commit message of new features should start with "feat: ",
fixes with "fix: ", refactorings with "refactor: " and tests with
"test: ".

.. _eldev: https://github.com/emacs-eldev/eldev

Documentation contributions
---------------------------

You don't need to be a developer to contribute! Contribution to the
documentation or code comments are very welcome. Please open a `Pull
Request`_ targeting the ``master`` branch with your proposed
modifications. To follow `Conventional
Commits 1.0.0 <https://www.conventionalcommits.org/en/v1.0.0/>`_, the
commit message should start with "docs: "

The documentation is written in reStructuredText. You'll need to
install `Sphinx <https://www.sphinx-doc.org>`_ to build it:

.. code-block:: bash

   python3 -m venv venv
   . venv/bin/activate # or activate.fish on fish
   pip3 install -r docs/requirements.txt

Then run :command:`eldev html` or :command:`eldev build turtles.info`
to build the documentation.

.. _Pull Request: https://github.com/szermatt/turtles/pulls
.. _Issue tracker: https://github.com/szermatt/turtles/issues
.. _discussion: https://github.com/szermatt/turtles/discussions
