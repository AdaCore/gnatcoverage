.. _GeneratingCoverageInformationFromLauterbachTrace32:

*******************************************************
Generating Coverage Information from Lauterbach Trace32
*******************************************************

Lauterbach Trace32 BranchFlow trace data can be produced while running an
executable on a processor supporting the needed execution trace output.

In environments where a Lauterbach probe is available for a target board with a
trace port, the Trace32 software can be used to control program loading and
execution on target hardware, and configure program trace data collection and
output. Trace32 APIs are provided for several languages (including Python and
C), and these are useful when not working in interactive mode.

Trace32 runs on a host computer that is connected via USB or Ethernet to a
hardware probe, which in turn is connected to a target processor.

Coverage analysis from Trace32 data involves four steps, requiring the specific
ability to export BranchFlow traces from Trace32:

 * Run the executable on the target using Lauterbach probe and Trace32;
 * Export BranchFlow trace with Trace32;
 * Invoke |gcvcnv| on the BranchFlow trace data file, producing an intermediate
   file;
 * Invoke |gcvcov| on this intermediate file.

While the program is running, trace data is sent over the processor's trace
interface (depending on the architecture) to the probe. The trace data is then
sent to Trace32 on the host computer to be exported to a file suitable for
|gcvcnv|.

Enabling Trace Data
===================

For Trace32 to produce BranchFlow execution traces, the target hardware and the
probe have to be configured properly. This configuration depends on the target
architecture and the specific device.

We recommend starting from one of the examples provided for your hardware in
the Trace32 demo folder, for instance
``demo/arm/hardware/stm32/stm32f4/sieve/stm32f4_discovery_offchip_trace.cmm``,
and to contact Lauterbach support for advice on the configuration for your
target.

Running the executable
======================

The executable can be loaded on the target using the Trace32 command
``Data.LOAD.auto <ELF_executable>``.

Before starting the program, a termination point has to be defined using
breakpoint. We recommend setting a breakpoint on the ``_exit`` function using
the Trace32 command ``Break.Set _exit``.

To run the program, use the ``Go.direct`` Trace32 command.

Exporting the Trace Data
========================

Once the program reached the termination breakpoint, the trace data can be
exported using the ``Trace.export.BranchFlow <filename> /NOSYMBOL /CALLER``
command. The last two options are required to produce a trace file in a
format that |gcvcnv| can translate.

The BranchFlow trace should then be converted to |gcp| format using |gcvcnv|
command (see :ref:`gnatcov_convert-commandline`).

Automation
==========

Trace32 provides an external API in C and Python to control the execution of
commands. This interface can be used to automate the process described above:
enable trace, run executable, export trace. Please refer to the Trace32
documentation for more information, for instance ``Controlling TRACE32 via
Python 3``.
