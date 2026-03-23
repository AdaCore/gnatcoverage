This test checks that "gnatcov coverage" can deal with SID and checkpoint files
produced in various cross-configuration scenarios.

Since we can't run the test on multiple hosts, we store compilation and trace
artifacts for each host instead of generating them every time the test is rerun.
If the format of these artifacts (traces, checkpoints) changes, the previously
stored format may become incompatible with the newer version of 'gnatcov.' In
such cases, we need to update the test to use artifacts in the newer format.

Updating the artifacts is necessary for both Windows (native) and Linux (native,
as well as the aarch64-elf and arm-elf targets). To do this, you'll need a
native toolchain for both hosts and a cross toolchain for arm-eabi and
aarch64-elf for Linux, with the instrumentation runtime setup.

In addition, also install the sshpass utility, available through apt:

```
apt install -y sshpass
```

In order for the update to work on windows, push the contents of your branch to
the eng/das/cov/gnatcoverage origin remote.

Then, to update instrumentation and trace artifacts for all hosts / targets,
run the `update.sh` script.
