This directory contains programs and data for running the gnatcov testsuite
tests in a particular way when using the iSystem Blue Box attached to a sytem
running the MPC5634M SOC.

`./testsuite.py` can be passed the argument:

```shell
--altrun=altrun/isys_nexus_5634
```

to cause:

* `setup.py` to be called, which builds `crun.exe` from `crun.adb`, which will
  be run in place of `gnatcov run` (with the same args that would normally be
  passed to `gnatcov run`).

* `pre_testsuite.py` to be called before the test sequence starts.

* `post_testsuite.py` to be called one all the tests are finished.

Crun creates a winIDEA workspace subdirectory (`isyswspace`) under the test
directory and populates it with the `justrun*` workspace and trace doc files,
and then calls `python get_trace.py` to run the target excutable and create a
nexus trace file in `isyswpace/nexus_trace.bin`. Then `crun` runs `gnatcov
convert` to convert the nexus trace into a gnatcov trace.
