from SUITE.context import thistest
from SUITE.control import target_info
from SUITE.cutils import Wdir
from SUITE.tutils import exepath_to, gprbuild, gprfor, xrun


Wdir("tmp_")

extralargs = ",--entry=volp" if target_info().partiallinks else ""
gprbuild(
    project=gprfor(["volp.adb"], srcdirs="../src"),
    extracargs="-ffunction-sections",
    largs="-Wl,--gc-sections" + extralargs,
)
xrun(exepath_to("volp"))

thistest.result()
