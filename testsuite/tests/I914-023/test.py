"""
This test is about a xcov/qemu misinteraction on very specific branch
sequences. We built once from the sources in src and re-exercise the binary.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import exepath_to, tracename_for, xcov, xrun


Wdir("tmp_")

# gprbuild(project = gprfor(['test_robots.adb']))

# The test intently gets into the 'Run' function twice in Cautious mode for a
# safe command.  We expect, then, partial coverage for both conditions in
#
#   procedure Run (R : in out Robot; C : Command) is
#      Mode : Opmode;
#   begin
#      Mode := Current_Mode (R);
#      if Mode = Cautious         <==
#        and then Unsafe (C)      <==
#
# We seek the reported status of the associated cond branch instructions
# directly:
xrun(exepath_to("../test_robots"))
xcov(
    [
        "coverage",
        "--level=branch",
        "--annotate=asm",
        tracename_for("test_robots"),
    ],
    "robots.out",
)

# Expect something like ...

#   17 +:       if Mode = Cautious
# [...]
# fffc01e8 >:  40 9e 00 20  bne-   cr7,0xfffc021c <robots__run+0000005c>
#   18 +:         and then Unsafe (C)
# [...]
# fffc0204 >:  40 9e 00 20  bne-   cr7,0xfffc0238 <robots__run+00000078>
thistest.fail_if(
    not match("fff003dc v:  40 9e 00 20      bne", "robots.out"),
    "expect Mode = Cautious always true",
)
thistest.fail_if(
    not match("fff003f8 v:  40 9e 00 48      bne", "robots.out"),
    "expect Unsafe (C) always false",
)

thistest.result()
