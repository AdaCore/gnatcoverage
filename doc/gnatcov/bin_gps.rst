######################
Using |gcp| from |gps|
######################

Starting from GNATcoverage version 22.0, binary traces (using gnatcov run) are
no longer supported in native profiles. This mode is still supported for cross
configurations, and the IDE integrated use in such cases is similar to the use
for source traces:

Go to the :menuselection:`Analyze --> Coverage --> GNATcoverage Binary Traces`
menu to perform each step individually, or use the ``Run All Actions`` item to
do everything at once.

In configurations where binary traces are not supported, the menu entry just
wouldn't show up.

You can also replace the toolbar button (dedicated to the instrumentation-based
scheme) with an alike button, but for binary traces. Go to GNATstudio
Preferences in :menuselection:`Edit --> Preferences`.

Then, go to the ``Build Targets`` section, and find the ``Run GNATcoverage``
workflow: add it to the toolbar as shown below.

.. image:: gps_screenshots/7-binary-traces.png

Remove the ``Run GNATcoverage with instrumentation`` entry from the toolbar in
the same fashion.

