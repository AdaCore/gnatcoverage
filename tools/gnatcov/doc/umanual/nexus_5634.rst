.. _GeneratingCoverageInformationFromNexusTraces:

*************************************************************
Generating Coverage Information from Nexus Traces on MPC5634M
*************************************************************

Nexus realtime Program Trace data can be produced while running an executable
on a processor supporting the needed combination of Nexus capabilites. Such
data can be processed by |gcp| in two steps:

 * Invoke |gcvcnv| on the Nexus trace data file, producing an intermediate file
 * Invoke |gcvcov| on this intermediate file

Currently, the freescale MPC5634M SOC is supported as a source of Nexus data,
using iSystem tools to interface with the processor and to produce a file
containing Nexus data.

The iSystem winIDEA software can be used to control program loading and execution
on target 5634 hardware, and, through the Analyzer tool, to configure and execute
Program Trace data collection and output. winIDEA is a GUI, and the information
in this section describes the user interaction with the GUI. winIDEA APIs are
provided for several languages (including Python and C++), and these are useful
when not working in interactive mode. 

winIDEA runs on a host computer that is connected via USB to iSystem hardware
(the "Blue Box"), which in turn is connected to a Mictor connector on the target
5634 processor through a Nexus cable.

While the program and analyzer are running, Program Trace data and other Nexus data
are sent over the processor's Nexus interface to the Blue Box, where it is annotated
and buffered. (The buffering is shared between memory on the Blue Box and resources
on the host computer; this affects how to configure the Analyzer). 
 
The Analyzer can stop as a result of any of several conditions; e.g. when the program
halts upon encountering a breakpoint, or when some condition set in the Analyzer
configuration is met.

Once the Analyzer has stopped, it should not be used to collect additional data in the
same buffer (the trace data will be inaccurate if there are gaps in its collection).
The Nexus data collected can be written to a file suitable for |gcvcnv| using the Analyzer's
``export`` function.

Nexus Program Trace Data: the Hardware Perspective
==================================================

The 5634 hardware supplies two options for controlling when Nexus Program Trace Data
is output while the CPU is running:

  * A bit in a control register in the Nexus Module
 
    When this bit is set, Nexus data will be produced whenever the CPU is running.

  * Watchpoint triggers
 
    If the control register bit is not set, Watchpoint Triggers may be used to generate trace data.
    Not all of the functionality of Watchpoint Triggers is supported by |gcp|, but what is used
    is built around the four Instruction Address Compare registers on the 5634 (IAC1 - IAC4).
    These registers can be used to cause watchpoint events when the PC attains specified values.
    In the basic usage, an address is set in one of the IAC registers and a watchpoint occurs when
    the PC matches the value set in the IAC. In the Nexus module, an IAC watchpoint event can be set
    to ``start`` or to ``stop`` the production of Nexus Program Trace messages. 
 
Configuring the winIDEA Analyzer
================================

The Watchpoint Triggers functionality of the 5634 is the (only) option supported by |gcp|.
The Analyzer must be configured accordingly, and options to the |gcvcnv| command reflect the
trigger settings that were set by the Analyzer when the trace data was collected: 

   * the IAC that was used for the ``start`` trigger, 
   * the address that was in the ``start`` trigger IAC, 
   * the IAC used for the ``stop`` trigger. 
 
The address in the ``stop`` trigger is not needed, and the ``stop`` trigger can be omitted completely. 
 
Here are two examples:

   * A program continually runs a loop where it receives a command and dispatches based on that command.
     The command input is implemented as a busy loop. To prevent a very large output of useless data from
     the busy loop, IAC1 is used as the ``start`` trigger, with the address set to the end of the busy loop,
     and IAC2 is set as the ``stop`` trigger, with its contents set to the beginning of the busy loop.

   * For coverage from unit testing, small programs are run to completion. In this scenario, ``start`` can be
     set at the entry point for ``main``. No ``stop`` trigger is needed, since a breakpoint is set upon exit
     from ``main``.

In the GUI, the hammer with a sheet of paper icon brings up the ``Analyzer Configuration List`` dialog,
and the hammer plus Trace icon brings up the ``Analyzer Configuration`` dialog. The following steps will
create a configuration that will be applicable across a wide range of test execution scenarios:

    * Open the ``Analyzer Configuration List`` dialog, choose New->Trace and choose a name for your configuration.
      This will bring up a trigger configuration window.
    
    * The first, ``Recorder`` tab is used to set values for four properites (click on the initial values for
      the first three to see that they are drop down lists). For ``Start``, ``Buffer Size`` and ``Trigger Position``
      choose ``Immediately``, ``1 GB`` (+/-) and ``Begin``. Do not check ``Break On Trigger``.

    * Select the ``MP5634M`` tab, and check ``Enabled``. IAC1 and IAC2 will be used for the ``start`` and
      ``stop`` triggers; check them and enter addresses. In the ``Record`` box, only ``Program`` should be
      selected, and for ``Start`` and ``Stop``, IAC1 and IAC2 should be chosen. ``Type`` needs to be
      ``Individual Branch Messages``, and Watchpoints should be ``All``. Click ``OK``.

        *Note*: The address set for the Start Trigger should not contain
        a branch instruction, since such a trigger may cause the Analyzer to
        lose the context for interpreting some Nexus messages.
        |gcvcnv| will generate an error message when it is passed a start
        trigger address for a branch instruction.
    
    * Back at the ``Analyzer Configuration`` window, the new configurataion will be shown to be the active one
      with a red arrow on the left. Check both ``Start Analyzer when CPU starts`` and
      ``Stop Analyzer when CPU stops``. The first is a minor convenience, making it unnecessary to explicitly
      start the Analyzer before performing the ``start`` from winIDEA.


Exporting the Trace Data
========================

To set the relevant arguments for the ``export`` command (whose icon looks like a diskette with a
green arrow):

 * In the window that pops up for ``export``, specify the file desired and choose ``Binary`` from
   the dropdown list for ``Format``;

 * Select ``Options...``, and in the resulting window, choose only ``On Chip Data``;

 * In the ``export`` window choose ``Entire Session`` for the ``Time Scope``;

 * Clicking ``OK`` will write the file; this may take a long time for large trace collections.
