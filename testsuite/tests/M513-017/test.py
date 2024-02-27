import os.path

from SCOV.map import MapChecker
from SUITE.context import thistest


MapChecker(["driver.adb"], alis=[os.path.join("obj", "lib.ali")]).run()
thistest.result()
