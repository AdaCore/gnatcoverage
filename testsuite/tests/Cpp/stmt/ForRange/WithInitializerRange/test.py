"""
Test that gnatcov correctly instruments a for range with an initializer as the
range expression and an initialization statement, e.g.
```
for (int i = 0; auto j : {1, 2}){}
```
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase().run()
thistest.result()
