# This holds "setup" code for the example altrun/ subdir.

# This is called before looking for binaries to setup testsuite and testcase
# hooks. Here, we produce one such binary from ada sources.

from gnatpython.ex import Run
import sys

p = Run (['gnatmake', '-f', '-g', '-p', '-Paltrun', "crun.adb"],
         output=sys.stdout)

if p.status != 0:
    sys.exit(1)

