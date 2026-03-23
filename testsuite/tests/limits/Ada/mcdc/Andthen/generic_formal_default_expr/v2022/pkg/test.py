from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest


TestCase(extracargs="-cargs:Ada -gnat2022").run(
    CovControl(instroptions="--ada 2022", auto_units=True)
)
thistest.result()
