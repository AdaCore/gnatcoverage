import os.path

from SUITE.control import target_info
from SUITE.context import env, thistest
from SUITE.cutils import Wdir, contents_of, match, text_to_file
from SUITE.tutils import (
    exepath_to,
    gprfor,
    gprbuild,
    tracename_for,
    xcov,
    xrun,
)


# This test checks that GNATcoverage properly matches padding instructions...
# and the ones that are not!
#
# A bit of context: on some platforms such as Windows, the executable binary
# format does not encode the length of symbols.  As a result, when extracting
# routines bytes, GNATcoverage also fetches instructions inserted by the linker
# to pad bytes before the next routine (for alignemnt purposes).  This triggers
# issues during consolidation as GNATcoverage assumes that all instances of a
# consolidated routine are binary equivalent: if one has padding bytes but not
# another one, GNATcoverage needs to know which instructions are there for
# padding so that it can ignore them.
#
# For instance: imagine we have the following routine instances:
# <foo>:
#   [regular instructions]
# <foo>:
#   [regular instructions]
#   nop
# When consolidating the two, GNATcoverage needs to acknowledge that the NOP
# can be discarded in order to have two routines with the same instructions.
#
# This testcase does exactly that: it has one reference assembly file got from
# the compilation of a C source (one assembly file per platform: see foo.c and
# foo-*.s).  This reference assembly file is built as a program and we also
# generate variants of this assembly file to embed various padding (and
# not-padding!) instructions. Then, we generate trace for all of these and we
# try to consolidate them: then we only have to check that GNATcoverage raises
# errors for variants with non-padding instructions and behaves fine with
# padding ones.


# As the set of padding instructions is completely architecture-dependent (we
# are talking about ISA), we need some testcase database. Upper-case ones come
# from the Intel Manual (look for: Recommended Multi-Byte Sequence of NOP
# Instruction).

insns_x86 = [
    # 1-byte padding
    (True, "nop"),
    # 2-bytes padding
    (True, "xchg %ax, %ax"),
    (False, "xchg %ax, %bx"),
    (True, "# 66 NOP\n" ".byte 0x66, 0x90"),
    # 3-bytes padding
    (True, "lea 0x0(%eax),%eax"),
    (True, "lea 0x0(%esi),%esi"),
    (False, "lea 0x0(%eax),%esi"),
    (False, "lea 0x1(%eax),%eax"),
    (False, "lea 0x0(%eax,%eax),%eax"),
    (False, "lea 0x0(%eax,%eax,2),%eax"),
    (True, "lea 0x0(%eax,2),%eax"),
    (True, "# NOP DWORD ptr [EAX]\n" ".byte 0x0F, 0x1F, 0x00"),
    # 4-bytes padding
    (True, "# lea    0x0(%esi,%eiz,1),%esi\n" ".byte 0x8d, 0x74, 0x26, 0x00"),
    (True, "# lea    0x0(%esi,%eiz,2),%esi\n" ".byte 0x8d, 0x74, 0x66, 0x00"),
    (False, "# lea    0x0(%ebp,%eiz,1),%esi\n" ".byte 0x8d, 0x74, 0x25, 0x00"),
    (False, "# lea    0x0(%esi,%esi,1),%esi\n" ".byte 0x8d, 0x74, 0x36, 0x00"),
    (True, "# NOP DWORD ptr [EAX + 00H]\n" ".byte 0x0F, 0x1F, 0x40, 0x00"),
    # 5-bytes padding
    (
        True,
        "# NOP DWORD ptr [EAX + EAX*1 + 00H]\n"
        ".byte 0x0F, 0x1F, 0x44, 0x00, 0x00",
    ),
    # 6-bytes padding
    (
        True,
        "# lea    0x0(%esi),%esi\n" ".byte 0x8d, 0xb6, 0x00, 0x00, 0x00, 0x00",
    ),
    (
        False,
        "# lea    0x0(%eax),%esi\n" ".byte 0x8d, 0xb0, 0x00, 0x00, 0x00, 0x00",
    ),
    (
        True,
        "# 66 NOP DWORD ptr [EAX + EAX*1 + 00H]\n"
        ".byte 0x66, 0x0F, 0x1F, 0x44, 0x00, 0x00",
    ),
    # 7-bytes padding
    (
        True,
        "# lea    0x0(%esi,%eiz,1),%esi\n"
        ".byte 0x8d, 0xb4, 0x26, 0x00, 0x00, 0x00, 0x00",
    ),
    (
        False,
        "# lea    0x0(%ebp,%eiz,1),%esi\n"
        ".byte 0x8d, 0xb4, 0x25, 0x00, 0x00, 0x00, 0x00",
    ),
    (
        False,
        "# lea    0x0(,%eiz,1),%esi\n"
        ".byte 0x8d, 0x34, 0x25, 0x00, 0x00, 0x00, 0x00",
    ),
    (
        True,
        "# lea    0x0(,%esi,1),%esi\n"
        ".byte 0x8d, 0x34, 0x35, 0x00, 0x00, 0x00, 0x00",
    ),
    (
        False,
        "# lea    0x0(,%edi,1),%esi\n"
        ".byte 0x8d, 0x34, 0x3d, 0x00, 0x00, 0x00, 0x00",
    ),
    (
        False,
        "# lea    0x0(%eax,%eax,1),%esi\n"
        ".byte 0x8d, 0xb4, 0x00, 0x00, 0x00, 0x00, 0x00",
    ),
    (
        True,
        "# NOP DWORD ptr [EAX + 00000000H]\n"
        ".byte 0x0F, 0x1F, 0x80, 0x00, 0x00, 0x00, 0x00",
    ),
    # 8-bytes padding
    (
        True,
        "# NOP DWORD ptr [EAX + EAX*1 + 00000000H]\n"
        ".byte 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00",
    ),
    # 9-bytes padding
    (
        True,
        "# 66 NOP DWORD ptr [EAX + EAX*1 + 00000000H]\n"
        ".byte 0x66, 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00",
    ),
]


test_database = {
    "x86-linux": insns_x86,
    "x86-windows": insns_x86,
}


def run_test(platform, insns):
    asm_src = os.path.abspath("foo-{}.s".format(platform))
    Wdir("tmp_")

    def build_material(name, padding_insn=None):
        tmp = Wdir("build-{}".format(name))

        asm_code = contents_of(asm_src).format(padding=padding_insn or "")
        asm_filename = text_to_file(asm_code, "foo.s")
        project = gprfor(
            [asm_filename], srcdirs=["."], objdir=".", langs=["Asm"]
        )
        # The assembly files already contain debug information: ask the
        # assembler not to produce it itself.
        gprbuild(project, extracargs="-g0")
        xrun(exepath_to("foo"))
        trace_name = os.path.abspath(tracename_for("foo"))

        tmp.to_homedir()
        return trace_name

    # First, build all test material: build programs (reference and
    # with-padding programs) and generate traces for them.

    trace_ref = build_material("ref")
    traces = []
    for i, (_, insn) in enumerate(insns):
        traces.append(build_material(str(i), insn))

    # Then try to consolidate each with-padding program with the reference one
    routine = target_info().to_platform_specific_symbol("foo")
    for i, ((is_padding, insn), trace_file) in enumerate(zip(insns, traces)):
        args = [
            "coverage",
            "--level=insn",
            "--annotate=asm",
            "--routines={}".format(routine),
            trace_ref,
            trace_file,
        ]
        out_file = "object-cons-{}.txt".format(i)
        p = xcov(args, out=out_file, register_failure=False)

        insn_label = insn.split("\n")[0]
        consolidation_failed = p.status != 0
        command = "gnatcov {}".format(" ".join(args))

        # First, if consolidation failed, make sure it is because of symbol
        # matching.
        thistest.fail_if(
            consolidation_failed
            and not match("TRACES_NAMES.CONSOLIDATION_ERROR", out_file),
            '[{}] "gnatcov coverage" failed in an unexpected way\n'
            "    Command was: {}".format(i, command),
        )

        # Now, check if GNATcoverage correctly matched inserted code as padding
        # (or as no padding).
        if is_padding:
            thistest.fail_if(
                consolidation_failed,
                '[{}] "{}" was not considered as padding (it is)\n'
                "    Command was: {}".format(i, insn_label, command),
            )
        else:
            thistest.fail_if(
                not consolidation_failed,
                '[{}] "{}" was considered as padding (it is not)\n'
                "    Command was: {}".format(i, insn_label, command),
            )


platform = env.target.platform
try:
    insns = test_database[platform]
except KeyError:
    thistest.comment("Nothing to test for {}".format(platform))
else:
    run_test(platform, insns)

thistest.result()
