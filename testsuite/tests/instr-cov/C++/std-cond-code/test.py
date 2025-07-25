"""
Check that gnatcov produces valid code when the code needs to be preprocessed
with the right preprocessor configuration.
"""

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, gprfor


def run_variant(
    variant_name,
    extra_instr_cppargs,
    extra_gprbuild_cargs,
    tolerate_instrument_messages=None,
    pred=lambda: True,
    skip_build=False,
):
    """
    Build and run the project with the given arguments, and check the predicate
    holds.

    :param variant_name: Name of the variant, used as the working dir name.
    :param extra_instr_cpp_args: Args passed to gnatcov instrument --c++-opts.
    :param extra_gprbuild_cargs: Args passed to gprbuild -cargs.
    :param pred: Predicate checked after building and running.
    :param skip_build: Whether to skip the build of the instrumented sources.
    """

    tmp = Wdir(f"tmp_{variant_name}")
    prj = gprfor(srcdirs=[".."], mains=["main.cpp"])
    xcov_instrument(
        gprsw=GPRswitches(root_project=prj),
        covlevel="stmt",
        extra_args=[f"--c++-opts={','.join(extra_instr_cppargs)}"],
        tolerate_messages=tolerate_instrument_messages,
        register_failure=False,
    )

    if not skip_build:
        gprbuild(prj, extracargs=extra_gprbuild_cargs, trace_mode="src")

    thistest.fail_if(
        not pred(), f"Unexpected assertion failure for variant {variant_name}"
    )

    tmp.to_homedir()


run_variant(
    variant_name="gprbuild-c++20",
    extra_instr_cppargs=[],
    extra_gprbuild_cargs=["-std=c++20", "-save-temps"],
    pred=lambda: "This is C++ code" in contents_of("obj/main.s"),
)
run_variant(
    variant_name="instr-c89",
    extra_instr_cppargs=["-std=c89"],
    extra_gprbuild_cargs=[],
    tolerate_instrument_messages="Failed to parse",
    pred=lambda: "Failed to parse" in contents_of("instrument.log"),
    skip_build=True,
)
run_variant(
    variant_name="instr-gprbuild-c++20",
    extra_instr_cppargs=["-std=c++20"],
    extra_gprbuild_cargs=["-std=c++20", "-save-temps"],
    pred=lambda: "This is C++20 code" in contents_of("obj/main.s"),
)

thistest.result()
