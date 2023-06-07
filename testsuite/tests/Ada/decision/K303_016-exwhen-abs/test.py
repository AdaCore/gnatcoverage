from SCOV.tc import TestCase
from SUITE.context import thistest
from SUITE.control import env


# Customer had specific extra options on ppc. We haven't been able to trigger
# a failure on this sample case anyway
if 'power' in env.target.triplet:
    target_options = '-mcpu=750 -mbig-endian -meabi -msdata=none -memb'
else:
    target_options = ''

TestCase(
    extracargs='-fdata-sections -ffunction-sections ' + target_options).run()

thistest.result()
