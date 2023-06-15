from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprbuild, xcov


wd = Wdir('tmp_')

gprbuild('../app.gpr')
xcov(args='map-routines -P../app', out='xcov.out')
output = contents_of('xcov.out')

thistest.fail_if(
    'no unit PAK2 in project App (coverage.units attribute)' not in output)
thistest.result()
