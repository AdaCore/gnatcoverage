import cProfile
import pstats
import time

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import exename_for, gprbuild, gprfor, xrun


Wdir('tmp_')
gpr = gprfor(srcdirs=['..'], mains=['p.adb'])


def run(action, repeat, tag):
    start = time.clock()
    for i in range(repeat):
        action()
    end = time.clock()
    print '%s: %2.5f' % (tag, end-start)


def eval_gprbuild():
    run(action=lambda: gprbuild(gpr), repeat=50, tag="gprbuild")


def eval_xrun():
    run(action=lambda: xrun(exename_for("p")), repeat=50, tag="xrun")


def eval(what):
    profiler = cProfile.Profile()
    profiler.run("eval_%s()" % what)

    ps = pstats.Stats(profiler)
    ps.strip_dirs()
    ps.sort_stats('time')

    print "==================="
    print "== %s ==" % what

    print "-- profiling stats --"
    ps.print_stats()

    print "-- callers --"
    ps.print_callers()

    print "-- callees --"
    ps.print_callees()


eval("gprbuild")
eval("xrun")

thistest.result()
