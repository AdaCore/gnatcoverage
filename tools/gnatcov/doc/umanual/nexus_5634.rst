**************************
Nexus Traces from MPC5634M
**************************

Nexus realtime Program Trace data can be produced while running an executable
on a processor supporting the needed combination of Nexus capabilites. Such
data can be imported into |gcp| by running |gcvcnv| on the collected
Nexus trace data, producing a trace file suitable for input to |gcvcov|.

Currently, the freescale MPC5634M SOC is supported as a source of Nexus data,
using iSystem tools to interface with the processor and to produce a file
containing Nexus data.

winIDEA Analyzer Settings
=========================

The iSystem program that is used for control of the loading and executing
programs on target 5634 HW is winIDEA. WinIDEA is also used for configuring
and executing Pogram Trace data collection and output, through the Analyzer
tool. WinIDEA is a GUI, and the directions provided here describe interaction
with the GUI, but there are winIDEA APIs provided for various languages including
python and C++, which are useful when not working in the experimental interactive
mode.

WinIDEA runs on a host computer, which is connected via USB to iSystem HW
(the Blue Box), which is connected to a Mictor connector on the target system
(with the 5634 processor) through a Nexus cable.

For our discussion here, we will presume that the executable has been loaded
onto the system containing the 5634, that the CPU has been reset and is
held in reset until the ``run`` command is given by winIDEA. Before ``run``
is executed, the Analyzer is configured and started. While the program is
running, Program Trace data and other Nexus data is sent over the processor's
Nexus interface to the Blue Box, where it is annotated and buffered (actually,
the buffering is shared between memory on the Blue Box, and resources on
the host computer, which can be helpful to understand when configuring the
Analyzer). The Analyzer can then stop due to any of several conditions; e.g.
when the program halts upon encountering a breakpoint, or when some condition set
in the Analyzer configuration is met.

After trace data is collected and the Analyzer has stopped, it can be instructed
to write data out to a file on the host system. The analyzer ``export`` function
(icon looks a diskette with a green arrow) is used to write out the file. There
are many choices for what to include in the file, but the proper choices for
use as input to |gcvcnv| are simple. In the window that pops up for ``export``,
specify the file desired and choose ``Binary`` from the dropdwon list for ``Format``.
Hit the ``Options...`` button, which will bring up another window. In that window,
the only option to include is ``On Chip Data``. OK that choice, and back in the
``export`` window choose ``Entire Session`` for the ``Time Scope``. Clicking
``OK`` will write the file, which can take a very long time for large trace
collections.

The Analyzer is configured using the ``Analyzer Configuration`` dialogs.
Some background about the Nexus module on the 5634 is helpful. There are 2
options for controlling when Nexus Program Trace Data is output while the
CPU is running. There is a bit in a control register in the Nexus Module
which, when set, causes Nexus data to be produced whenever the CPU is running.
If that bit is not set, an alternative method of controlling Nexus message
generation is available. That method is the use of Watchpoint Triggers. Not all
of the flexibility of Watchpoint Triggers is supported by |gcp|, but what is
used is built around the 4 Instruction Address Compare registers on the 5634
(IAC1 - IAC4). These registers can be used to cause watchpoint events
when the PC attains specified values. In the basic usage, an address is set
in one of the IAC registers and a watchpoint occurs when the PC matches the
value set in the IAC. In the Nexus module, and IAC watchpoint event can be
set to ``start`` or to ``stop`` the production of Nexus Program Trace messages.
That is the method of controlling Nexus Program Trace messages that is understood
by |gcvcnv|, and |gcvcnv| must be told specific settings of the Analyzer that were
used when the program was run by winIDEA. In particular, the IAC that was used
for the ``start`` trigger, the address that was in the ``start`` trigger IAC, and
the IAC used for the ``stop`` trigger. The address in the ``stop`` trigger isn't
needed, and the ``stop`` trigger can be left out completely. A brief mention of
a couple examples:

* A large program with a busy loop in the middle of it is being run. To prevent
  a very large output of useless data from the busy loop, IAC1 is used as the
  `` start`` trigger, with the address set to the end of the busy loop, and IAC2
  is set as the ``stop`` trigger, with its contents set to the begining of the
  busy loop.

* For coverage from unit testing, small programs are run completely through. For
  this, ``start`` can be set at the entry point for ``main``, and no ``stop``
  trigger is needed, as a breakpoint is set upon exit from ``main``.


In the GUI, the hammer
with a sheet of paper icon brings up the ``Analyzer Configuration List`` dialog, and
hammer plus Trace icon brings up the ``Analyzer Configuration`` dialog.
