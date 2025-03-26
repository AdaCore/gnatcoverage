#####################
GNATtest User's Guide
#####################

.. role:: switch(samp)

.. |rightarrow| unicode:: 0x2192

.. _gnattest:

.. index:: ! gnattest

``gnattest`` tool is a utility that creates unit-test skeletons
as well as a test driver infrastructure (harness). ``gnattest`` creates
a skeleton for each visible subprogram in the packages under consideration when
they do not exist already.

``gnattest`` is a project-aware tool. A project file is mandatory for test
driver generation.
The project file package that can specify
``gnattest`` switches is named ``gnattest``.

The user can choose to generate a single test driver
that will run all individual tests, or separate test drivers for each test. The
second option allows much greater flexibility in test execution environment,
allows to benefit from parallel tests execution to increase performance, and
provides stubbing support.

``gnattest`` also has a mode of operation where it acts as the test
aggregator when multiple test executables must be run, in particular when
the separate test drivers were generated. In this mode it handles individual
tests execution and upon completion reports the summary results of the test
run.

In order to process source files from a project, ``gnattest`` has to
semantically analyze the sources. Therefore, test skeletons can only be
generated for legal Ada units. If a unit is dependent on other units,
those units should be among the source files of the project or of other projects
imported by this one. Note that it is no longer necessary to specify the Ada
language version; ``gnattest`` can process Ada source code written in any
version from Ada 83 onward without specifying any language version switch.

Generated skeletons and harnesses are based on the AUnit testing framework.
AUnit is an Ada adaptation of the xxxUnit testing frameworks, similar to JUnit
for Java or CppUnit for C++. While it is advised that gnattest users read
the AUnit manual, deep knowledge of AUnit is not necessary for using ``gnattest``.
For correct operation of ``gnattest``, AUnit should be installed and
aunit.gpr must be on the project path. Except for some special circumstances
(e.g. a custom run-time is used), this should normally be the case out of the box.


.. _Running_gnattest:

Running ``gnattest``
--------------------

There are two ways of running ``gnattest``.

.. _Framework_Generation_Mode:

Framework Generation Mode
^^^^^^^^^^^^^^^^^^^^^^^^^

In this mode ``gnattest`` has the following command-line interface:

  ::

      $ gnattest -Pprojname [ switches ] [ filename ]

where

* :switch:`-P{projname}`
    specifies the project defining the location of source files. When no
    file names are provided on the command line, all sources in the project
    are used as input. This switch is required.
    
    For the semantics of aggregate project processing by gnattest, see the
    :ref:`aggregate_projects` section.

* :switch:`{filename}`
    is the name of the source file containing the library unit package *declaration*
    (the package "spec") for which a test package will be created. The file name
    may be given with a path.

* :samp:`{switches}`
    is an optional sequence of switches as described below.


``gnattest`` results can be found in two different places.

* *automatic harness*:
    This is the harness code, which is located by default in
    "gnattest/harness" directory created in the object directory of
    the main project file. All of this code is generated completely
    automatically and can be destroyed and regenerated at will, with the
    exception of the file *gnattest_common.gpr*, which is created if absent,
    but never overwritten. It is not recommended to modify other files
    manually, since these modifications will be lost if ``gnattest`` is re-run.
    The entry point in the harness code is
    the project file named *test_driver.gpr*. Tests can be compiled and run
    using a command such as:

    ::

       $ gprbuild -P<harness-dir>/test_driver

    Note that if you need to adjust any options used to compile the harness,
    you can do so by editing the file *gnattest_common.gpr*.

* *actual unit test skeletons*:
    A test skeleton for each visible subprogram is created in a separate file, if it
    doesn't exist already. By default, those separate test files are located in a
    "gnattest/tests" directory that is created in the object directory of
    corresponding project file. For example, if a source file my_unit.ads in
    directory src contains a visible subprogram Proc, then the corresponding unit
    test will be found in file src/tests/my_unit-test_data-tests.adb and will be
    called Test_Proc_<code>. <code> is a signature encoding used to differentiate
    test names in case of overloading.

    Note that if the project already has both my_unit.ads and my_unit-test_data.ads,
    this will cause a name conflict with the generated test package.


.. _Test_Execution_Mode:

Test Execution Mode
^^^^^^^^^^^^^^^^^^^

In this  mode ``gnattest`` has a the following command-line interface:

  ::

      $ gnattest test_drivers.list [ switches ]

where

* :samp:`{test_drivers.list}`
     is the name of the text file containing the list of executables to treat as
     test drivers. This file is automatically generated by gnattest, but can be
     hand-edited to add or remove tests. This switch is required.


* :samp:`{switches}`
     is an optional sequence of switches as described below.


.. _Switches_for_gnattest_in_framework_generation_mode:

Switches for ``gnattest`` in framework generation mode
------------------------------------------------------

  .. index:: --strict (gnattest)

:switch:`--strict`
  Return error exit code if there are any compilation errors.


  .. index:: -q (gnattest)

:switch:`-q`
  Quiet mode: suppresses noncritical output messages.


  .. index:: -v (gnattest)

:switch:`-v`
  Verbose mode: produces additional output about the execution of the tool.
  When specified alone on the command line, prints tool version and exits.


  .. index:: -U (gnattest)
  .. index:: -r (gnattest)

:switch:`-r, -U`
  Recursively considers all sources from all projects.


  .. index:: -U (gnattest)

:switch:`-U {source_file}`
  Process only those source files for units in the closure of
  the Ada source contained in ``source_file``. Note that this option
  expects the source file name but not the Ada unit name as its
  parameter.


  .. index:: --no-subprojects (gnattest)

:switch:`--no-subprojects`
  Process only source files from the root project.


  .. index:: -X (gnattest)

:switch:`-X{name}={val}`
  Indicates that the external variable ``name`` in the project has the
  value ``val``.


  .. index:: -files (gnattest)

:switch:`-files={filename}`
  Take as arguments the files listed in text file ``file``.
  Text file ``file`` may contain empty lines that are ignored.
  Each nonempty line should contain the name of an existing file.
  Several such switches may be specified simultaneously.


  .. index:: --ignore (gnattest)

:switch:`--ignore={filename}`
  Do not process the sources listed in a specified file.


  .. index:: --RTS (gnattest)

:switch:`--RTS={rts-path}`
  Specifies the default location of the runtime library. For restricted
  profiles, ``gnattest`` takes into account the run-time limitations when
  generating the harness.


  .. index:: --additional-tests (gnattest)

:switch:`--additional-tests={projname}`
  Sources described in ``projname`` are considered potential additional
  manual tests to be added to the test suite.


  .. index:: --harness-only (gnattest)

:switch:`--harness-only`
  When this option is given, ``gnattest`` creates a harness for all
  sources, treating them as test packages. This option is not compatible with
  closure computation done by -U main.


  .. index:: --separate-drivers (gnattest)

:switch:`--separate-drivers[={val}]`
  Generates a separate test driver for each test or unit under test, rather
  than a single executable incorporating all tests. ``val`` can be "unit" or
  "test", or may be omitted, which defaults to "unit".


  .. index:: --stub (gnattest)

:switch:`--stub`
  Generates the testing framework that uses subsystem stubbing to isolate the
  code under test.


  .. index:: --recursive-stub (gnattest)

:switch:`--recursive-stub`
  Used along --stub, indicates gnattest to generate stubs for all the packages
  that are withed by the stubbed units, recursively.


  .. index:: --harness-dir (gnattest)

:switch:`--harness-dir={dirname}`
  Specifies the directory that will hold the harness packages and project file
  for the test driver. If the ``dirname`` is a relative path, it is considered
  relative to the object directory of the project file.


  .. index:: --tests-dir (gnattest)

:switch:`--tests-dir={dirname}`
  All test packages are placed in the ``dirname`` directory.
  If the ``dirname`` is a relative path, it is considered relative to the object
  directory of the project file. When all sources from all projects are taken
  recursively from all projects, ``dirname`` directories are created for each
  project in their object directories and test packages are placed accordingly.


  .. index:: --subdirs (gnattest)

:switch:`--subdirs={dirname}`
  Test packages are placed in a subdirectory of the corresponding source
  directory, with the name ``dirname``. Thus, each set of unit tests is located
  in a subdirectory of the code under test. If the sources are in separate
  directories, each source directory has a test subdirectory named ``dirname``.


  .. index:: --tests-root (gnattest)

:switch:`--tests-root={dirname}`
  The hierarchy of source directories, if any, is recreated in the ``dirname``
  directory, with test packages placed in directories corresponding to those
  of the sources.
  If the ``dirname`` is a relative path, it is considered relative to the object
  directory of the project file. When projects are considered recursively,
  directory hierarchies of tested sources are
  recreated for each project in their object directories and test packages are
  placed accordingly.


  .. index:: --stubs-dir (gnattest)

:switch:`--stubs-dir={dirname}`
  The hierarchy of directories containing stubbed units is recreated in
  the ``dirname`` directory, with stubs placed in directories corresponding to
  projects they are derived from.
  If the ``dirname`` is a relative path, it is considered relative to the object
  directory of the project file. When projects are considered recursively,
  directory hierarchies of stubs are
  recreated for each project in their object directories and test packages are
  placed accordingly.


  .. index:: --exclude-from-stubbing (gnattest)

:switch:`--exclude-from-stubbing={filename}`
  Disables stubbing of units listed in ``filename``. The file should contain
  corresponding spec files, one per line.


:switch:`--exclude-from-stubbing:{spec}={filename}`
  Same as above, but corresponding units will not be stubbed only when testing
  unit whose specification is declared in specified ``spec`` file.


  .. index:: --validate-type-extensions (gnattest)

:switch:`--validate-type-extensions`
  Enables substitution check: run all tests from all parents in order
  to check substitutability in accordance with the Liskov substitution principle (LSP).


  .. index:: --inheritance-check (gnattest)

:switch:`--inheritance-check`
  Enables inheritance check: run inherited tests against descendants.


  .. index:: --no-inheritance-check (gnattest)

:switch:`--no-inheritance-check`
  Disables inheritance check.


  .. index:: --no-inheritance-check (gnattest)

:switch:`--test-case-only`
  Generates test skeletons only for subprograms that have at least one
  associated pragma or aspect Test_Case.


  .. index:: --skeleton-default (gnattest)

:switch:`--skeleton-default={val}`
  Specifies the default behavior of generated skeletons. ``val`` can be either
  "fail" or "pass", "fail" being the default.


  .. index:: --passed-tests (gnattest)

:switch:`--passed-tests={val}`
  Specifies whether or not passed tests should be shown. ``val`` can be either
  "show" or "hide", "show" being the default.


  .. index:: --exit-status (gnattest)

:switch:`--exit-status={val}`
  Specifies whether or not generated test driver should return failure exit
  status if at least one test fails or crashes. ``val`` can be either
  "on" or "off", "off" being the default. If ``--exit-status=on`` is used to
  generate the test harness, it should also be used if running the test
  drivers via the ``gnattest test_drivers.list`` command.


  .. index:: --omit-sloc (gnattest)

:switch:`--omit-sloc`
  Suppresses comment line containing file name and line number of corresponding
  subprograms in test skeletons.


  .. index:: --no-command-line (gnattest)

:switch:`--no-command-line`
  Don't add command line support to test driver. Note that regardless of this
  switch, ``gnattest`` will automatically refrain from adding command
  line support if it detects that the selected run-time doesn't provide
  this capability.


  .. index:: --test-duration (gnattest)

:switch:`--test-duration`
  Adds time measurements for each test in generated test driver.


  .. index:: --reporter (gnattest)

:switch:`--reporter={val}`
  Use specified reporter in the test driver. ``val`` is expected to be a name
  of child package of AUnit.Reporter. Test drivers generated with non-default
  reporter specified cannot be properly processed by test execution mode of ``gnattest``.


:switch:`--tests-root`, :switch:`--subdirs` and :switch:`--tests-dir` switches are mutually exclusive.


.. _Switches_for_gnattest_in_test_execution_mode:

Switches for ``gnattest`` in test execution mode
------------------------------------------------


  .. index:: --passed-tests (gnattest)

:switch:`--passed-tests={val}`
  Specifies whether or not passed tests should be shown. ``val`` can be either
  "show" or "hide", "show" being the default.

:switch:`--exit-status={val}`
  Specifies whether or not generated test driver should return failure exit
  status if at least one test fails or crashes. ``val`` can be either
  "on" or "off", "off" being the default. The switch ``--exit-status=on``
  should be used both when generating the test harness and when running the
  test drivers via the ``gnattest test_drivers.list`` command.

  .. index:: --queues (gnattest)
  .. index:: -j (gnattest)

:switch:`--queues={n}`, :switch:`-j{n}`
  Runs ``n`` tests in parallel (default is 1).


  .. index:: --copy-environment (gnattest)

:switch:`--copy-environment={dir}`
  Contents of ``dir`` directory will be copied to temporary directories
  created by gnattest in which individual test drivers are spawned.

  .. index:: --subdirs (gnattest)

:switch:`--subdirs={dirname}`
  Test driver executables from ``test_drivers.list`` are searched in
  ``dirname`` subdirectories of specified locations.


.. _Project_Attributes_for_gnattest:

Project Attributes for ``gnattest``
-----------------------------------

Most of the command-line options can also be passed to the tool by adding
special attributes to the project file. Those attributes should be put in
package ``Gnattest``. Here is the list of attributes:


* ``Tests_Root``
     is used to select the same output mode as with the ``--tests-root`` option.
     This attribute cannot be used together with ``Subdir`` or ``Tests_Dir``.

* ``Subdir``
     is used to select the same output mode as with the ``--subdirs`` option.
     This attribute cannot be used together with ``Tests_Root`` or ``Tests_Dir``.

* ``Tests_Dir``
     is used to select the same output mode as with the ``--tests-dir`` option.
     This attribute cannot be used together with ``Subdir`` or ``Tests_Root``.

* ``Stubs_Dir``
     is used to select the same output mode as with the ``--stubs-dir`` option.

* ``Harness_Dir``
     is used to specify the directory in which to place harness packages and project
     file for the test driver, otherwise specified by ``--harness-dir``.

* ``Additional_Tests``
     is used to specify the project file, otherwise given by
     ``--additional-tests`` switch.

* ``Skeletons_Default``
     is used to specify the default behaviour of test skeletons, otherwise
     specified by ``--skeleton-default`` option. The value of this attribute
     should be either ``pass`` or ``fail``.

* ``Default_Stub_Exclusion_List``
     is used to specify the file with list of units whose bodies should not
     be stubbed, otherwise specified by ``--exclude-from-stubbing=filename``.

* ``Stub_Exclusion_List ("spec")``
     is used to specify the file with list of units whose bodies should not
     be stubbed when testing "spec", otherwise specified by
     ``--exclude-from-stubbing:spec=filename``.

Each of those attributes can be overridden from the command line if needed.
Other ``gnattest`` switches can also be passed via the project
file as an attribute list called ``Gnattest_Switches``.


.. _Simple_gnattest_Example:

Simple Example
--------------

Let's take a very simple example using the first ``gnattest`` example
located in:

  ::

      <install_prefix>/share/examples/gnattest/simple

This project contains a simple package containing one subprogram. By running ``gnattest``:

  ::

      $ gnattest --harness-dir=driver -Psimple.gpr

a test driver is created in directory ``driver``. It can be compiled and run:

  ::

     $ cd obj/driver
     $ gprbuild -Ptest_driver
     $ test_runner

One failed test with the diagnosis "test not implemented" is reported.
Since no special output option was specified, the test package ``Simple.Tests``
is located in:

  ::

      <install_prefix>/share/examples/gnattest/simple/obj/gnattest/tests


For each package containing visible subprograms, a child test package is
generated. It contains one test routine per tested subprogram. Each
declaration of a test subprogram has a comment specifying which tested
subprogram it corresponds to. Bodies of test routines are placed in test package
bodies and are surrounded by special comment sections. Those comment sections
should not be removed or modified in order for gnattest to be able to regenerate
test packages and keep already written tests in place.
The test routine ``Test_Inc_4f8b9f`` located at :file:`simple-test_data-tests.adb` contains
a single statement: a call to procedure ``Assert``. It has two arguments:
the Boolean expression we want to check and the diagnosis message to display if
the condition is false.

That is where actual testing code should be written after a proper setup.
An actual check can be performed by replacing the ``Assert`` call with:

  ::

      Assert (Inc (1) = 2, "wrong incrementation");

After recompiling and running the test driver, one successfully passed test
is reported.


.. _Setting_Up_and_Tearing_Down_the_Testing_Environment:

Setting Up and Tearing Down the Testing Environment
---------------------------------------------------

Besides test routines themselves, each test package has a parent package
``Test_Data`` that has two procedures: ``Set_Up`` and ``Tear_Down``. This package is never
overwritten by the tool. ``Set_Up`` is called before each test routine of the
package, and ``Tear_Down`` is called after each test routine. Those two procedures
can be used to perform necessary initialization and finalization,
memory allocation, etc. Test type declared in ``Test_Data`` package is parent type
for the test type of test package and can have user-defined components whose
values can be set by ``Set_Up`` routine and used in test routines afterwards.


.. _Regenerating_Tests:

Regenerating Tests
------------------

Bodies of test routines and ``Test_Data`` packages are never overridden after they
have been created once. As long as the name of the subprogram, full expanded Ada
names and order of its parameters are the same, and comment sections are
intact, the old test routine will fit in its place and no test skeleton will be
generated for the subprogram.

This can be demonstrated with the previous example. By uncommenting declaration
and body of function Dec in ``simple.ads`` and ``simple.adb``, running
``gnattest`` on the project, and then running the test driver:

  ::

      $ gnattest --harness-dir=driver -Psimple.gpr
      $ cd obj/driver
      $ gprbuild -Ptest_driver
      $ test_runner

The old test is not replaced with a stub, nor is it lost, but a new test
skeleton is created for function ``Dec``.

The only way of regenerating tests skeletons is to remove the previously created
tests together with corresponding comment sections.


.. _Default_Test_Behavior:

Default Test Behavior
---------------------

The generated test driver can treat unimplemented tests in two ways:
either count them all as failed (this is useful to see which tests are still
left to implement) or as passed (to sort out unimplemented ones from those
actually failing).

The test driver accepts a switch to specify this behavior:
:switch:`--skeleton-default={val}`, where ``val`` is either ``pass`` or ``fail`` (exactly as for
``gnattest``).

The default behavior of the test driver is set with the same switch
as passed to ``gnattest`` when generating the test driver.

Passing it to the driver generated on the first example:

  ::

      $ test_runner --skeleton-default=pass

makes both tests pass, even the unimplemented one.


.. _Testing_Primitive_Operations_of_Tagged_Types:

Testing Primitive Operations of Tagged Types
--------------------------------------------

Creation of test skeletons for primitive operations of tagged types entails
a number of features. Test routines for all primitives of a given tagged type
are placed in a separate child package named according to the tagged type. For
example, if you have tagged type ``T`` in package ``P``, all tests for primitives
of ``T`` will be in ``P.T_Test_Data.T_Tests``.

Consider running ``gnattest`` on the second example (note: actual tests for this
example already exist, so there's no need to worry if the tool reports that
no new stubs were generated):

  ::

      $ cd <install_prefix>/share/examples/gnattest/tagged_rec
      $ gnattest --harness-dir=driver -Ptagged_rec.gpr

Taking a closer look at the test type declared in the test package
*Speed1.Controller_Test_Data* is necessary. It is declared in:

  ::

      <install_prefix>/share/examples/gnattest/tagged_rec/obj/gnattest/tests

Test types are direct or indirect descendants of
*AUnit.Test_Fixtures.Test_Fixture* type. In the case of non-primitive tested
subprograms, the user doesn't need to be concerned with them. However,
when generating test packages for primitive operations, there are some things
the user needs to know.

Type ``Test_Controller`` has components that allow assignment of various
derivations of type ``Controller``. And if you look at the specification of
package *Speed2.Auto_Controller*, you will see that ``Test_Auto_Controller``
actually derives from ``Test_Controller`` rather than AUnit type ``Test_Fixture``.
Thus, test types mirror the hierarchy of tested types.

The ``Set_Up`` procedure of ``Test_Data`` package corresponding to a test package
of primitive operations of type ``T`` assigns to ``Fixture`` a reference to an
object of that exact type ``T``. Note, however, that if the tagged type has
discriminants, the ``Set_Up`` only has a commented template for setting
up the fixture, since filling the discriminant with actual value is up
to the user.

The knowledge of the structure of test types allows additional testing
without additional effort. Those possibilities are described below.


.. _Testing_Inheritance:

Testing Inheritance
-------------------

Since the test type hierarchy mimics the hierarchy of tested types, the
inheritance of tests takes place. An example of such inheritance can be
seen by running the test driver generated for the second example. As previously
mentioned, actual tests are already written for this example.

  ::

      $ cd obj/driver
      $ gprbuild -Ptest_driver
      $ test_runner

There are 6 passed tests while there are only 5 testable subprograms. The test
routine for function Speed has been inherited and run against objects of the
derived type.


.. _Tagged_Type_Substitutability_Testing:

Tagged Type Substitutability Testing
------------------------------------

*Tagged Type Substitutability Testing* is a way of verifying the global type
consistency by testing. Global type consistency is a principle stating that if
``S`` is a subtype of ``T`` (in Ada, ``S`` is a derived type of tagged type ``T``),
then objects of type ``T`` may be replaced with objects of type ``S`` (that is,
objects of type ``S`` may be substituted for objects of type ``T``), without
altering any of the desirable properties of the program. When the properties
of the program are expressed in the form of subprogram preconditions and
postconditions (let's call them pre and post), the principle is formulated as
relations between the pre and post of primitive operations and the pre and post
of their derived operations. The pre of a derived operation should not be
stronger than the original pre, and the post of the derived operation should
not be weaker than the original post. Those relations ensure that verifying if
a dispatching call is safe can be done just by using the pre and post of the
root operation.

Verifying global type consistency by testing consists of running all the unit
tests associated with the primitives of a given tagged type with objects of its
derived types.

In the example used in the previous section, there was clearly a violation of
type consistency. The overriding primitive ``Adjust_Speed`` in package ``Speed2``
removes the functionality of the overridden primitive and thus doesn't respect
the consistency principle.
``gnattest`` has a special option to run overridden parent tests against objects
of the type which have overriding primitives:

  ::

      $ gnattest --harness-dir=driver --validate-type-extensions -Ptagged_rec.gpr
      $ cd obj/driver
      $ gprbuild -Ptest_driver
      $ test_runner

While all the tests pass by themselves, the parent test for ``Adjust_Speed`` fails
against objects of the derived type.

Non-overridden tests are already inherited for derived test types, so the
``--validate-type-extensions`` enables the application of overridden tests
to objects of derived types.


.. _Testing_with_Contracts:

Testing with Contracts
----------------------

``gnattest`` supports pragmas ``Pre``, ``Post``, and ``Test_Case``,
as well as the corresponding Ada 2012 aspects.
Test routines are generated, one per each ``Test_Case`` associated with a tested
subprogram. Those test routines have special wrappers for tested functions
that have composition of pre- and postcondition of the subprogram with
"requires" and "ensures" of the ``Test_Case`` (depending on the mode, pre and post
either count for ``Nominal`` mode or do *not* count for ``Robustness`` mode).

The third example demonstrates how this works:

  ::

      $ cd <install_prefix>/share/examples/gnattest/contracts
      $ gnattest --harness-dir=driver -Pcontracts.gpr

Putting actual checks within the range of the contract does not cause any
error reports. For example, for the test routine which corresponds to
test case 1:

  ::

      Assert (Sqrt (9.0) = 3.0, "wrong sqrt");

and for the test routine corresponding to test case 2:

  ::

      Assert (Sqrt (-5.0) = -1.0, "wrong error indication");

are acceptable:

  ::

      $ cd obj/driver
      $ gprbuild -Ptest_driver
      $ test_runner

However, by changing 9.0 to 25.0 and 3.0 to 5.0, for example, you can get
a precondition violation for test case one. Also, by using any otherwise
correct but positive pair of numbers in the second test routine, you can also
get a precondition violation. Postconditions are checked and reported
the same way.


.. _Additional_Tests:

Additional Tests
----------------

``gnattest`` can add user-written tests to the main suite of the test
driver. ``gnattest`` traverses the given packages and searches for test
routines. All procedures with a single in out parameter of a type which is
derived from *AUnit.Test_Fixtures.Test_Fixture* and that are declared in package
specifications are added to the suites and are then executed by the test driver.
(``Set_Up`` and ``Tear_Down`` are filtered out.)

An example illustrates two ways of creating test harnesses for user-written
tests. Directory ``additional_tests`` contains an AUnit-based test driver written
by hand.

  ::

      <install_prefix>/share/examples/gnattest/additional_tests/

To create a test driver for already-written tests, use the ``--harness-only``
option:

  ::

      gnattest -Padditional/harness/harness.gpr --harness-dir=harness_only \\
        --harness-only
      gprbuild -Pharness_only/test_driver.gpr
      harness_only/test_runner

Additional tests can also be executed together with generated tests:

  ::

      gnattest -Psimple.gpr --additional-tests=additional/harness/harness.gpr \\
        --harness-dir=mixing
      gprbuild -Pmixing/test_driver.gpr
      mixing/test_runner


.. _Individual_Test_Drivers:

Individual Test Drivers
-----------------------

By default, ``gnattest`` generates a monolithic test driver that
aggregates the individual tests into a single executable. It is also possible
to generate separate executables for each test or each unit under test, by
passing the switch ``--separate-drivers`` with corresponding parameter. This
approach scales better for large testing campaigns, especially involving target
architectures with limited resources typical for embedded development. It can
also provide a major performance benefit on multi-core systems by allowing
simultaneous execution of multiple tests.

``gnattest`` can take charge of executing the individual tests; for this,
instead of passing a project file, a text file containing the list of
executables can be passed. Such a file is automatically generated by gnattest
under the name :file:`test_drivers.list`, but it can be
hand-edited to add or remove tests, or replaced. The individual tests can
also be executed standalone, or from any user-defined scripted framework.


.. _Stubbing:

Stubbing
--------

Depending on the testing campaign, it is sometimes necessary to isolate the
part of the algorithm under test from its dependencies. This is accomplished
via *stubbing*, i.e. replacing the subprograms that are called from the
subprogram under test by stand-in subprograms that match the profiles of the
original ones, but simply return predetermined values required by the test
scenario.

This mode of test harness generation is activated by the switch ``--stub``.

The implementation approach chosen by ``gnattest`` is as follows.
For each package under consideration all the packages it is directly depending
on are stubbed, excluding the generic packages and package instantiations.
The stubs are shared for each package under test. The specs of packages to stub
remain intact, while their bodies are replaced, and hide the original bodies by
means of extending projects. Also, for each stubbed
package, a child package with setter routines for each subprogram declaration
is created. These setters are meant to be used to set the behavior of
stubbed subprograms from within test cases.

Note that subprograms belonging to the same package as the subprogram under
test are not stubbed. This guarantees that the sources being tested are
exactly the sources used for production, which is an important property for
establishing the traceability between the testing campaign and production code.

Due to the nature of stubbing process, this mode implies the switch
``--separate-drivers``, i.e. an individual test driver (with the
corresponding hierarchy of extending projects) is generated for each unit under
test.

.. note::

   Developing a stubs-based testing campaign requires
   good understanding of the infrastructure created by ``gnattest`` for
   this purpose. We recommend following the two stubbing tutorials
   ``simple_stubbing`` and ``advanced_stubbing`` provided
   under :file:`<install_prefix>/share/examples/gnattest` before
   attempting to use this powerful feature.


.. _Gnatcov_Integration:

Integration with GNATcoverage
-----------------------------

In addition to the harness, ``gnattest`` generates a Makefile. This Makefile
provides targets for building the test drivers and also the targets for
computing the coverage information using GNATcoverage framework when this
coverage analysis tool is available. The target ``coverage`` fully automates
the process: it will first build all test drivers, then run them under
GNATcoverage, analyze individual trace files, and finally aggregate them:

  ::

      make coverage

For more details about using GNATtest with GNATcoverage see :ref:`Integration_Part`.

.. _Putting_Tests_under_Version_Control:

Putting Tests under Version Control
-----------------------------------

As has been stated earlier, ``gnattest`` generates two different types
of code, test skeletons and harness. The harness is generated completely
automatically each time, does not require manual changes and therefore should
not be put under version control.
It makes sense to put under version control files containing test data packages,
both specs and bodies, and files containing bodies of test packages. Note that
test package specs are also generated automatically each time and should not be
put under version control.
Option ``--omit-sloc`` may be useful when putting test packages under version control.


.. _aggregate_projects:

Aggregate project handling
--------------------------

If the project passed to ``gnattest`` with the ``-P`` switch is an aggregate
project, the aggregated projects will be processed sequentially and
independently. This will result in one harness directory being generated by
default, in the object directories of each of the aggregated projects.

``gnattest`` will not generate any project file or makefile to automate the
build of the harnesses of each of the aggregated project.

Artifact directories and aggregate projects
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, all artifacts generated by ``gnattest`` are located in
subdirectories of the object directory of each of the aggregated projects. This
in particular means that tests or stubs for a common dependency of two
aggregated projects will be duplicated. In order to avoid this, options such as
``--stubs-dir``., ``--tests-dir`` or ``--subdirs`` can be used, with **relative
paths** so that the artifacts for the common dependencies are generated in the
same location, and re-used across each test harness.

On the contrary, using ``--harness-dir`` or ``--tests-dir`` with an absolute
path will result in the harness and/or files of a first aggregated project being
overwritten by the generation of the test harness for subsequent aggregated
projects, and should thus be avoided.

.. _Current_Limitations:

Current Limitations
-------------------

The tool currently has the following limitations:

* generic tests for nested generic packages and their instantiations are
  not supported;
* tests for protected subprograms and entries are not supported;
* pragma ``No_Run_Time`` is not supported;
* pragma ``No_Secondary_Stack`` is not supported;
* if pragmas for interfacing with foreign languages are used, manual
  adjustments might be necessary to make the test harness compilable;
* use of some constructs, such as elaboration-control pragmas, Type_Invariant
  aspects, and complex variable initializations that use Subprogram'Access,
  may result in elaboration circularities in the generated harness;
* heavy usage of preprocessor that affects constructs like subprogram profiles
  or tagged type hierarchies may result in improper test driver generation.

.. _Automatic_testcase_generation:

Automatically generating test cases (experimental)
--------------------------------------------------

Please note that all the features described bellow are experimental, and the
interface is subject to change.

GNATtest has the capability to generate test inputs for subprograms under test.
This test generation feature is also useable in conjunction with GNATfuzz, in
order to use GNATtest harnesses (generated or manually written) as a starting
corpus for a fuzzing session, and to integrate inputs of interest found by
GNATfuzz back into the test harness. For more details, see section
:ref:`Gnattest_Gnatfuzz`.

.. _Tgen_Env:

Setting up the test generation runtime
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Generation of values for Ada cannot be fully done statically, as the bounds of
some types may only be defined at runtime. As such, the test generation feature
requires the compilation and installation of a runtime project.

The sources for that project are located at
``<GNATdas_install_dir/share/tgen/tgen_rts``.

To build the runtime, simply copy the above directory to a location of you
choice, build the project using ``gprbuild``, install it using ``gprinstall``
and make it available to the tools by referencing it in the ``GPR_PROJECT_PATH``
environment variable:

.. code-block:: sh

  # Clean previous source if present
  rm -rf /tmp/tgen_rts_src

  # Copy the sources
  cp -r <GNATdas_install_dir>/share/tgen/tgen_rts /tmp/tgen_rts_src

  # Build the project
  cd /tmp/tgen_rts_src
  gprbuild -P tgen_rts.gpr

  # Install the project (removing the previous one if needed)
  gprinstall --uninstall -P tgen_rts.gpr --prefix=/tmp/tgen_rts_install
  gprinstall -p -P tgen_rts.gpr --prefix=/tmp/tgen_rts_install

  # Make it available to other tools
  export GPR_PROJECT_PATH=/tmp/tgen_rts_install/share/gpr:$GPR_PROJECT_PATH

Generating test inputs
^^^^^^^^^^^^^^^^^^^^^^

``gnattest`` provides a ``--gen-test-vectors`` switch that can be used to
automatically generate test cases for all of the supported subprogram profiles.
The number of generated test cases can be configured through the
``--gen-test-num`` switch.

``gnattest`` can automatically generate test cases unless any of the following
are true:

1. Any of the subprogram's "in" or "in out" mode parameters are of an
   Access type or contain a sub-component of an Access type.
2. Any of the subprogram's "in" or "in out" mode parameters are Subprogram
   Access Types.
3. Any of the subprogram's "in" or "in out" mode parameters are Limited types.
4. Any of the subprogram's "in" or "out" mode parameters are tagged types.
5. Any of the subprogram's "in" or "out" mode parameters is a private type of
   a nested package.

Input value generation currently follows a simple strategy for each input
parameter of the subprogram under test. Parameters of scalar types, and scalar
components of composite types have their values uniformly generated. For
unconstrained array types, a length is randomly chosen between 0 and 10
elements, then the low bound is randomly chosen and the high bound computed
accordingly to those two first points.

For record discriminants, different strategies are chosen depending on the use
of the discriminant within the record: If the discriminant constraints a array
component, then the array strategy described above is used. If the discriminant
is used in a variant part, generation will be biased in order to generated all
possible shapes of the record (i.e. explore all variants). Otherwise, these are
generated as any other scalar component.


The generated test cases are then stored in a ad-hoc (and yet to be specified)
JSON format, in files under the ``<obj_dir>/gnattest/tests/JSON_Tests`` directory.
The generated JSON files are preserved through a ``gnattest`` rerun. The user is
thus free to modify them, to e.g. fill in expected return values, though
backward compatibility of the format is not guaranteed at this stage.

``gnattest`` also generates Ada files to actually execute the test cases. Each test vector
has its own AUnit test case, and all test cases for a specific subprogram are all
stored in a dedicated file, namely
``<unit_name>-test_data-test_<subp_name>_<subp_hash>.ad[bs]``, where
``<unit_name>`` is the name of the unit in which the subprogram is declared,
``<subp_name>`` is the name of the subprogram, and <subp_hash> is a hash based
on the profile of the subprogram, in order to differentiate overloads.

The content of these files are re-generated each time ``gnattest`` is invoked,
independently of the presence of the ``--gen-test-vectors`` switch on the
command line. It is thus not necessary to re-invoke ``gnattest`` with that
switch more than once, unless the goal is to generate additional test inputs.

..
   TODO: document the --unparse switch
