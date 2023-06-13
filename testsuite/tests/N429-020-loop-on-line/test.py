import os.path

from SCOV.map import MapChecker
from SUITE.tutils import thistest

MapChecker(
    ['main.c'],
    alis=[os.path.join('obj', 'main.c.gli')]
).run()
thistest.result()
