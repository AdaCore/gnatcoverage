import datetime

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov

tmp = Wdir("tmp_")

prj = gprfor(srcdirs=[".."], mains=["main.adb"])

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-a", "xml"],
)

# Check that by default, date is displayed in local time
xcov(xcov_args)
timezone_offset = datetime.datetime.now().astimezone().strftime("%z")
formatted_offset = timezone_offset[:3] + ":" + timezone_offset[3:5]
thistest.fail_if(
    formatted_offset not in contents_of("obj/trace.xml"),
    "Date displayed in xml report should be specified "
    "as local time by default.",
)

# Check that passing explicitely local displays the date under the right format
xcov(xcov_args + ["--timezone=local"])
thistest.fail_if(
    formatted_offset not in contents_of("obj/trace.xml"),
    "Date displayed in xml report does not have the right "
    "annotation (time offset expected).",
)

# Check that passing explicitely UTC displays the date under the right format
xcov(xcov_args + ["--timezone=utc"])
thistest.fail_if(
    "UTC" not in contents_of("obj/trace.xml"),
    "Date displayed in xml report does not have the right "
    "annotation (UTC expected).",
)

thistest.result()
