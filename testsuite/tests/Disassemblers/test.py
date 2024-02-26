"""
Test the integration of libopcodes in gnatcov's disassembling machinery.

gnatcov delegates object code disassembling to binutils's libopcodes, so it
makes no sense here to test for all possible instructions. What we rather check
is whether the integration of libopcodes in gnatcov works as expected.

To do so, we compare the output of `gnatcov disassemble` against baselines for
assembly files corresponding to the current target architecture, in both
endianity when applicable. Testcase should demonstrate that generally
libopcodes is properly used, and that symbolization works as expected.
"""

from collections import OrderedDict
import os.path

from e3.diff import diff

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor, xcov


# Mapping: architecture discriminant -> tests directory.  If one discriminant
# is a key for this mapping, the associated directory names are used to get
# testcases.


class Testcase(object):
    def __init__(self, test_dir, discriminant, cargs=(), asm_spec=""):
        self.test_dir = test_dir
        self.discriminant = discriminant
        self.cargs = cargs
        self.asm_spec = asm_spec

        self.spec_file = None

    def prepare(self):
        if self.asm_spec:
            self.spec_file = os.path.abspath("{}.spec".format(self.test_dir))
            with open(self.spec_file, "w") as f:
                f.write("*asm:\n")
                f.write("+ {}\n".format(self.asm_spec))

            self.cargs = list(self.cargs) + [
                "-specs={}".format(self.spec_file)
            ]


testcases = OrderedDict()

for tc in (
    # Avoid running this on Windows platforms as the assembler will fail
    # processing the test material, there.
    Testcase("x86", "x86-linux"),
    Testcase("x86_64", "x86_64-linux"),
    Testcase("lmp", "visium-elf", ["-mcpu=gr6"]),
    # Use GCC's spec files, in particular the "asm" rule to force the
    # assemblers' endianity. This short-circuits the -m{little,big}-endian flag
    # we get from the testuite-wide project configuration file.
    Testcase("arm_le", "arm", ["-mcpu=cortex-r4f"], "-EL"),
    Testcase("arm_be", "arm", ["-mcpu=cortex-r4f"], "-EB"),
    Testcase("thumb_le", "arm", ["-mthumb"], "-EL"),
    Testcase("thumb_be", "arm", ["-mthumb"], "-EB"),
    Testcase("ppc_be", "ppc-elf", ["-mbig-endian"]),
    Testcase("ppc_le", "ppc-elf", ["-mlittle-endian"]),
    Testcase("leon", "leon3-elf"),
):
    testcases[tc.test_dir] = tc


def with_ext(name, ext):
    """Return the given name suffixed by an extension."""
    return "{}{}{}".format(name, os.path.extsep, ext)


def is_asm(filename):
    """Return if the given filename is an assembly source."""
    return os.path.splitext(filename)[-1] == ".s"


tmp = Wdir("tmp_")

# Run tests in each test directory that matches architecture discriminants.
for test_dir, tc in testcases.items():
    if tc.discriminant not in thistest.options.tags:
        continue
    tc.prepare()

    path = os.path.join(tmp.homedir, test_dir)
    tmp_sub = Wdir(test_dir)

    testcases = list(sorted(filter(is_asm, os.listdir(path))))

    # Prepare object file from assembly sources.
    project = gprfor(testcases, srcdirs=path, main_cargs=tc.cargs)
    gprbuild(project, gargs=["-c"])

    # And then iterate on each testcase to run it.
    for testcase in testcases:
        compile_unit = os.path.splitext(testcase)[0]
        objfile = os.path.join("obj", with_ext(compile_unit, "o"))

        baseline = os.path.join(path, with_ext(compile_unit, "baseline"))
        disa_gnatcov = with_ext(compile_unit, "disassembly")

        # Disassemble it using gnatcov.
        xcov(["disassemble", objfile], out=disa_gnatcov)

        # And finally compare it to the baseline.
        disaconv_diff = diff(baseline, disa_gnatcov)
        thistest.log(
            "Comparing the disassembly of {}/{}...".format(test_dir, testcase)
        )
        if disaconv_diff:
            thistest.log(disaconv_diff)
        else:
            thistest.log("No difference")
        thistest.fail_if(
            disaconv_diff,
            "{}/{}: disassemblies are not the same".format(test_dir, testcase),
        )

    tmp_sub.to_homedir()

thistest.result()
