from SCOV.tc import *
from SUITE.cutils import list_to_tmp, Wdir
from SUITE.gprutils import gprdep_for

libdep = os.path.abspath("mylib/mylib.gpr")

TestCase().run (
    covcontrol = CovControl (
        deps = [libdep],
        scoptions = "-Pgen --recursive"
        )
    )

thistest.result()
