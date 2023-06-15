"""
Check that gnatcov produces valid code when the code needs to be preprocessed
with the right preprocessor configuration.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


def run_variant(variant_name, extra_instr_cppargs, extra_gprbuild_cargs,
                tolerate_instrument_messages=None, pred=lambda: True):
    """
    Build and run the project with the given arguments, and check the predicate
    holds.

    :param variant_name: Name of the variant, used as the working dir name.
    :param extra_instr_cpp_args: Args passed to gnatcov instrument --c++-opts.
    :param extra_gprbuild_cargs: Args passed to gprbuild -cargs.
    :param pred: Predicate checked after building and running.
    """

    tmp = Wdir(f"tmp_{variant_name}")
    build_and_run(
        gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."],
                                              mains=["main.cpp"])),
        covlevel="stmt",
        mains=["main"],
        extra_instr_args=[f"--c++-opts={','.join(extra_instr_cppargs)}"],
        extra_gprbuild_cargs=extra_gprbuild_cargs,
        trace_mode="src",
        tolerate_instrument_messages=tolerate_instrument_messages,
        extra_coverage_args=[],
        register_failure=False,
    )

    thistest.fail_if(
        not pred(),
        f"Unexpected assertion failure for variant {variant_name}"
    )

    tmp.to_homedir()


run_variant(
    variant_name="gprbuild-c++20",
    extra_instr_cppargs=[],
    extra_gprbuild_cargs=["-std=c++20"],
    pred=lambda: "This is C++ code" in contents_of("main_output.txt"),
)
run_variant(
    variant_name="instr-c89",
    extra_instr_cppargs=["-std=c89"],
    extra_gprbuild_cargs=[],
    tolerate_instrument_messages="Failed to parse",
    pred=lambda: "Failed to parse" in contents_of("instrument.log"),
)
run_variant(
    variant_name="instr-gprbuild-c++20",
    extra_instr_cppargs=["-std=c++20"],
    extra_gprbuild_cargs=["-std=c++20"],
    pred=lambda: "This is C++20 code" in contents_of("main_output.txt"),
)

thistest.result()
