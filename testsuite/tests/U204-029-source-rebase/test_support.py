import os
import shutil
from typing import Iterable

from e3.fs import cp, mkdir

from SCOV.minicheck import (
    CovReport,
    build_run_and_coverage,
    check_xcov_reports,
)
from SUITE.cutils import Wdir, contents_of, empty
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


def out_of_source_checkpoint(
    variant_name: str,
    main: str,
    units: list[str],
    origin_src_dir: str,
    suppress_src_dir: bool,
) -> tuple[str, str]:
    """
    Create a checkpoint with a copy of the sources in origin_src_dir, using
    main as the main program and units as the list of units of interest.

    This operates on a copy of the given sources, the copies are deleted after
    the checkpoint creation if suppress_src_dir is True, so that gnatcov cannot
    find the source in their original location when producing a report.

    Returns the name of the created checkpoint and the absolute path to the
    directory where the sources were when creating the checkpoint.
    """
    local_src_dir = f"src_{variant_name}"
    cp(origin_src_dir, local_src_dir, recursive=True)
    checkpoint_name = f"{main}.ckpt"

    build_run_and_coverage(
        gprsw=GPRswitches(
            gprfor(mains=[main], srcdirs=local_src_dir, objdir="obj"),
            units=units,
        ),
        covlevel="stmt",
        mains=[os.path.splitext(main)[0]],
        extra_coverage_args=[f"--save-checkpoint={checkpoint_name}"],
    )

    if suppress_src_dir:
        shutil.rmtree(local_src_dir)

    return (checkpoint_name, os.path.abspath(local_src_dir))


def consolidate_and_check(
    variant_basename: str,
    expected_xcov_results: CovReport,
    expect_failure: bool,
    checkpoints: Iterable[str],
    rebase_opts: list[str],
    output_dir_name: str,
) -> None:
    """
    Consolidate the given set of checkpoints and create xcov reports.
    The xcov reports are checked against expected_xcov_results, and if
    expect_failure is True, check that gnatcov emits warnings concerning files
    not found. Otherwise check that gnatcov does not emmit any warnings.
    """
    log_filename = f"coverage-{output_dir_name}.log"
    mkdir(output_dir_name)
    xcov(
        ["coverage", "-cstmt", "-axcov", f"--output-dir={output_dir_name}"]
        + [f"-C{checkpoint}" for checkpoint in checkpoints]
        + rebase_opts,
        out=log_filename,
        tolerate_messages=".",
    )

    if expect_failure:
        thistest.fail_if_no_match(
            "'gnatcov coverage' output"
            f" ({variant_basename}-{output_dir_name})",
            r"^(warning: can't open .*\n)+$",
            contents_of(log_filename),
        )
    else:
        thistest.fail_if(
            not empty(log_filename),
            f"'gnatcov coverage' output ({variant_basename}-{output_dir_name})"
            " not empty:\n  " + contents_of(log_filename),
        )

    check_xcov_reports(output_dir_name, expected_xcov_results)


def run_variant(
    variant_basename: str,
    mains_list: list[str],
    units_lists: list[list[str]],
    origin_src_dir: str,
    expected_xcov_results: CovReport,
    rebase_dir: str | None = None,
    expect_failure: bool = False,
    suppress_src_dir: bool = False,
) -> None:
    """
    Create a set of checkpoints using, for checkpoint i, the sources in
    origin_src_dir, the main in mains_list[i] and the units in units_lists[i].
    If suppress_src_dir is True, remove the created source directory that is a
    copy of origin_src_dir.

    Try to rebase the source to rebase_dir if specified, or to origin_src_dir
    otherwise.
    """
    origin_src_dir = os.path.abspath(origin_src_dir)

    wd = Wdir(f"tmp_{variant_basename}")
    # If no rebase_dir is specified, use origin_src_dir as the place to search
    # for sources after rebase.
    if rebase_dir is None:
        rebase_dir = origin_src_dir
    else:
        rebase_dir = os.path.abspath(rebase_dir)

    # Create a checkpoint for each set of main/units_of_interest and retrieve
    # the checkpoint's name, as well as the path to the source directory used
    # to create said checkpoint.
    checkpoints, prefixes = zip(
        *[
            out_of_source_checkpoint(
                f"{variant_basename}_{i}",
                main,
                units,
                origin_src_dir,
                suppress_src_dir,
            )
            for i, (main, units) in enumerate(zip(mains_list, units_lists))
        ]
    )

    # Test individual options passed to gnatcov for each checkpoint
    consolidate_and_check(
        variant_basename,
        expected_xcov_results,
        expect_failure,
        checkpoints,
        rebase_opts=[
            f"--source-rebase={prefix}={rebase_dir}" for prefix in prefixes
        ],
        output_dir_name="simple",
    )

    # Test using one globbing pattern to specify source rebase for all
    # checkpoints.
    glob_pattern = os.path.join(wd.homedir, "*", "src_*")
    consolidate_and_check(
        variant_basename,
        expected_xcov_results,
        expect_failure,
        checkpoints,
        rebase_opts=[f"--source-rebase={glob_pattern}={rebase_dir}"],
        output_dir_name="globbing_pattern",
    )

    # Test using a response file to specify source rebase for all checkpoints
    response_file_name = "src-rebase.txt"

    with open(response_file_name, "w") as f:
        f.writelines([f"{prefix}={rebase_dir}\n" for prefix in prefixes])

    consolidate_and_check(
        variant_basename,
        expected_xcov_results,
        expect_failure,
        checkpoints,
        rebase_opts=[f"--source-rebase=@{response_file_name}"],
        output_dir_name="response_file",
    )

    wd.to_homedir()
