from e3.diff import diff
from e3.os.process import Run

from SUITE.context import env, thistest
from SUITE.cutils import Wdir, list_to_file
from SUITE.tutils import xcov


Wdir("tmp_")

sfiles_for = {
    "leon-elf": ["leon-elf-orphans.s", "leon-elf-emptys.s"],
    "powerpc-elf": ["powerpc-elf-orphans.s", "powerpc-elf-emptys.s"],
}


def do_for(target):
    def to_object(sfile):
        ofile = sfile.replace(".s", ".o")
        Run([target + "-gcc", "-c", "../" + sfile, "-o", ofile])
        return ofile

    ofiles = [to_object(sfile) for sfile in sfiles_for[target]]

    out_actual = target + "-actual.out"
    out_expected = "../" + target + "-expected.out"

    xcov(["scan-objects"] + ofiles, out=out_actual, tolerate_messages=".")

    thistest.fail_if(
        diff(out_actual, out_expected),
        "%s != %s, using explicit list" % (out_actual, out_expected),
    )

    xcov(
        ["scan-objects", "@" + list_to_file(ofiles)],
        out=out_actual,
        tolerate_messages=".",
    )

    thistest.fail_if(
        diff(out_actual, out_expected),
        "%s != %s, using list file argument" % (out_actual, out_expected),
    )


do_for(env.target.triplet)

thistest.result()
