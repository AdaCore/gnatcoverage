.. _bin_traces:

#####################################
Producing binary traces with |gcvrun|
#####################################

When the project context allows it, the simplest way to produce binary traces
for |gcp| is to run the program in an environment that knows how to produce
such traces directly. For native Linux/Windows configurations or
cross/bareboard environments operating within |gem|, the |gcvrun| command is
available for this purpose.

.. toctree::
   :maxdepth: 2

   run_prereq
   run_commandline
   run_control
   trace_format
