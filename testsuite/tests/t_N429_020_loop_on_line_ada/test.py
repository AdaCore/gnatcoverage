import os.path

from SCOV.map import MapChecker
from SUITE.tutils import thistest

MapChecker(["foo.adb"], alis=[os.path.join("obj", "foo.ali")]).run()
thistest.result()
