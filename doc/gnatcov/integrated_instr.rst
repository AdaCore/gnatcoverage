.. _integrated_instr:

######################################################################
Producing source traces with integrated instrumentation (experimental)
######################################################################

|gcv| provides an alternate way of instrumenting sources, available for C/C++
under linux and when using gcc or g++ as a compiler. Theoretically, any build
system should be supported, but it has only been tested for the Makefile / CMake
build systems so far.

As with the workflow involving a separate instrumentation, a :term:`coverage
runtime <Coverage Runtime>` to be used by the instrumented code needs to be
setup as a required prerequisite. Refer to the :ref:`instr-rts` section of this
manual for a description of this step. The installed instrumentation runtime
must be visible on the ``GPR_PROJECT_PATH``.

Then the process essentially goes like:

#. Setup the integrated instrumentation process, specifying the instrumentation
   parameters (units of interest, coverage level etc.) and the compilers of use.
#. Build the instrumented code using the generated compiler wrapper(s);
#. Execute the program to produce a trace.

The compiler wrapper embeds knowledge through the ``gnatcov_config.json``
configuration file that is generated alongside it. It is thus necessary to
re-run the setup step it if the closure of sources of interest, or if the build
environment (e.g. the path for the original compiler) changes.


Specifying instrumentation switches
===================================

The user specifies the switches through the ``gnatcov setup-integration``
command, which accepts any option accepted by ``gnatcov instrument``, except the
ones using project mechanisms to filter out units of interest: ``--units`` and
``--projects``. Refer to :ref:`src_traces` for more information regarding the
source instrumentation specific switches.

As there is no project acting as a units of interest provider, the user must
explicitly pass files of interest through the ``--files`` switch, which expects
a list of full filenames that can be passed through the command line, or using a
response file.

To generate a compiler wrapper, use the ``--compilers`` switch. The list of
supported compilers is ``gcc`` and ``g++``.

This will generate in the current directory, or in the directory specified by
the ``--output-dir`` switch an executable compiler wrapper and a
``gnatcov_config.json`` file along. Note that this configuration file is used to
configure various instrumentation options: it is thus very important that it
resides in the same directory as the compiler wrapper.


Building an instrumented executable
===================================

The build process can be run unchanged after it has been configured to use the
generated compiler wrapper instead of the original compiler.


A simple Makefile example
=========================

The sources for the following example can be found under the
``share/examples/gnatcoverage/doc/integrated`` directory of the GNATDAS
distribution.

The following considers that the instrumentation runtime was previously
installed with ``gnatcov setup``, and that the ``GPR_PROJECT_PATH`` variable
contains its installed location. See :ref:`instr-rts`.

Let's consider a simple ``main.cpp`` file:

.. code-block:: c++

   #include <iostream>

   int main(int argc, char **argv){
     std::cout << "Hello World" << std::endl;
     return 0;
   }

and the following Makefile:

.. code-block:: makefile

   CC=g++
   OBJ = main.o

   %.o: %.c
	   $(CC) -c -o $@ $<

   test: $(OBJ)
	   $(CC) -o $@ $^

We start by configuring the instrumentation process:

.. code-block:: sh

   cd <my-project>
   gnatcov setup-integration --files=<my_project>/main.cpp --compilers=g++

Then, we launch the build processed unchanged, with the compiler wrapper first
on the path:

.. code-block:: sh

   export PATH=<my-project>:$PATH
   make

This will produce an instrumented executable, that will produce a source trace
when run, that can be analyzed with ``gnatcov coverage``.

A simple CMake example
======================

The sources for the following example can be found under the
``share/examples/gnatcoverage/doc/integrated`` directory of the GNATDAS
distribution. To ensure that the Makefile provided with the example sources
uses CMake as a build system, specify ``BUILD_SYSTEM=CMake`` on the `make`
invocation.

The following considers that the instrumentation runtime  was installed through
the use of ``gnatcov setup``.

Let's consider a simple ``main.cpp`` file

.. code-block:: c++

   #include <iostream>

   int main(int argc, char **argv){
     std::cout << "Hello World" << std::endl;
     return 0;
   }

The CMakeLists.txt file to be used to compile the main.cpp file is :

.. code-block:: cmake

   cmake_minimum_required(VERSION 3.5)
   project(HelloWorld)

   add_executable(hello_world main.cpp)

We start by creating the build directory, and configuring the instrumentation
process there:

.. code-block:: sh

   cd <my-project>
   mkdir build
   cd build
   gnatcov setup-integration --files=<my_project>/main.cpp --compilers=g++

This creates a ``g++`` compiler wrapper in the build directory, along with a
``gnatcov_config.json`` file that we intend to use as a proxy for compilation.
To do that, we have to configure the CMake build process accordingly, using the
``CMAKE_CXX_COMPILER`` variable. We run the configuration command in the build
directory:

.. code-block:: sh

   cmake .. -DCMAKE_CXX_COMPILER=<my_project>/build/g++

The default generator for CMake is "Unix Makefiles", so we can then run the
build process with ``make``, and our executable which will produce a source trace
that can be analyzed by ``gnatcov coverage``.
