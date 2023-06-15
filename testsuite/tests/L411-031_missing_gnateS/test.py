import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import exepath_to, gprbuild, tracename_for, xcov, xrun


Wdir("tmp_")

# The intent of this test is to check that gnatcov warns when it is queried to
# perform source coverage analysis on a unit for which a critical compilation
# option was not used. We exercise various combinations of option selections.
critical_opts = ("-g", "-fdump-scos", "-fpreserve-control-flow")

# We need full control over the compilation options here, so disconnect
# all the default ones
_cargs = {'scovcargs': False,
          'suitecargs': False}

gprtemplate = """
project %(name)s is
  for Source_Dirs use ("../src");
  for Object_Dir use "obj";
  for Exec_Dir use ".";
  for Languages use ("Ada");
  package Compiler is
    for Default_Switches ("Ada") use (%(opts)s);
  end Compiler;
end %(name)s;
"""


def trywith(thisid, thisopts):
    gprname = thisid+".gpr"
    with open(gprname, 'w') as gpr:
        gpr.write(gprtemplate % {
            "name": thisid,
            "opts": ','.join('"%s"' % opt for opt in thisopts)})

    gprbuild(gprname, gargs=["test_assert.adb"], **_cargs)
    xrun(exepath_to("test_assert"))
    out = thisid+".out"
    ali = "obj/checks.ali"
    xcov(['coverage', '--level=stmt+decision', '--annotate=xcov',
          '--scos=%s' % ali, tracename_for("test_assert")],
         out=out)

    # Check that we get warning for each critical option not in the set we
    # passed. For warning checks purposes, equate -gnateS to -fdump-scos.
    thisopts = ["-fdump-scos" if opt == "-gnateS" else opt
                for opt in thisopts]
    out = contents_of(out)

    for opt in critical_opts:
        if opt not in thisopts:
            thistest.fail_if(
                not re.search("warning: %s: unit compiled without .*%s"
                              % (ali, opt), out),
                'case "%s" missing warning on %s' % (thisid, opt))


# Now exercise the variants, with shortcut for option names
g = "-g"
p = "-fpreserve-control-flow"
d = "-fdump-scos"

trywith(thisid="g", thisopts=[g])
trywith(thisid="p", thisopts=[p])
trywith(thisid="d", thisopts=[d])

trywith(thisid="pdg", thisopts=[p, d, g])

trywith(thisid="gp", thisopts=[g, p])
trywith(thisid="gd", thisopts=[g, d])
trywith(thisid="pd", thisopts=[p, d])

# Check that -gnateS can be used as a replacement for -fdump-scos
trywith(thisid="epg", thisopts=["-gnateS", p, g])

thistest.result()
