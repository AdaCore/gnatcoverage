.. _rust_cov:

************************************
Rust coverage analysis with |gcvcov|
************************************

Source coverage analysis for the Rust language is an unstable feature of
|gcv|. It relies on the internal coverage instrumentation of the rustc
compiler, which is itself leaning on the code coverage framework of the
LLVM project.

.. note::
  This feature is made to be compatible with the **GNATPro For Rust**
  tool suite, compatibility with other rust toolchains is not guaranteed.

.. _rust_cov_workflow:

Rust coverage workflow
======================

The workflow consists in 3 steps. First, build the program while instrumenting
it for coverage analysis. The instrumentation is delegated to ``rustc``.
Then, run the program, which produces trace files. Finally, use |gcvcov| to
generate a coverage report.

.. _rust_cov_workflow_1:

Instrument and build the project
--------------------------------

Unlike source coverage analysis for Ada, C and C++ which rely on |gcv| to
instrument the source code *before* compiling it, the Rust workflow delegates
instrumentation to the ``rustc`` compiler.

To instrument the code, pass the :cmd-option:`-Cinstrument-coverage` argument
to the compiler. If instrumenting for the ``stmt+decision`` or ``stmt+mcdc``
coverage levels, pass the :cmd-option:`-Ccoverage-options=mcdc` additionally.

Decision coverage as defined by |gcv| does not exist in the rust compiler,
thus |gcv| will infer the decision coverage from MC/DC coverage.

The example showcases using the ``Cargo`` build system to instrument a project
for ``stmt+mcdc`` analysis analysis.

::

   RUSTFLAGS="-Cinstrument-coverage -Ccoverage-options=mcdc" cargo build

.. _rust_cov_workflow_2:

Run the executed binary
-----------------------

Once again, the example showcases running the compiled program through
``Cargo``. At the end of execution, a trace file ending with ``.profraw`` is
created.

The name of the file can be customized with the ``LLVM_PROFILE_FILE``
environment variable.
See `LLVM_PROFILE_FILE format <https://releases.llvm.org/19.1.0/tools/clang/
docs/SourceBasedCodeCoverage.html#running-the-instrumented-program>`_.

::

   LLVM_PROFILE_FILE="trace-%p.profraw" cargo run -- ARGS...

The program may be run several times with different inputs to improve final
coverage.

.. _rust_cov_workflow_3:

Produce a coverage report
-------------------------

To produce the coverage analysis from the obtained ``.profraw`` files, proceed
as follow:

::

   gnatcov coverage --level=stmt+mcdc -a xcov --exec EXEC_BIN PROFRAW_FILES...

The important part is that the program binary needs to be passed with the
:cmd-option:`--exec` argument. In a ``Cargo`` project, the binary usually lives
under ``target/debug`` or ``target/release``.

Note that aside of producing a report, the rust coverage report can be saved
as a :ref:`checkpoint<checkpoints>` and reused for consolidation.

.. _rust_cov_limitations:

Rust coverage limitations
=========================

This feature being relatively new, there are known limitations that we shall
mention hereafter.

MC/DC false-negatives related to instrumentation level
--------------------------------------------------------

While the |gcvins| workflow controls and tracks the instrumentation level
of artifacts, making it possible to raise an error when trying to perform
MC/DC analysis on an artifact that was not instrumented for MC/DC, the Rust
workflow does not have such a safety feature yet.

It means that if the project is instrumented without MC/DC support, and
the analysis is done with :cmd-option:`--level=stmt+mcdc`, no MC/DC violation
will be raised, even though there could have been some if the program had been
instrumented adequately.

Approximative source location ranges
------------------------------------

Currently, you may witness that some SCOs have a source location range that
can overlap or merge with its neighbours. This is due to bugs in the
compiler's instrumentation logic and will be fixed in coming releases.

Unsupported constructs
----------------------

Some constructs are not yet supported:

- MC/DC instrumentation of pattern matching. Pattern matching can be viewed
  as a decision with multiple conditions. However, it can lead to more than
  just 2 outcomes.

- Rust ``macros by example`` and procedural macros are not yet instrumented.

