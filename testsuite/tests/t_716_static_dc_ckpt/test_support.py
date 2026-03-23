from __future__ import annotations

import abc
from enum import Enum
import os
import os.path
import shutil
from typing import ClassVar

from SCOV.minicheck import (
    CovReport,
    build_run_and_coverage,
    check_xcov_reports,
)
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


class PkgType(Enum):
    """
    This enum describes the possible values with which we can replace a
    condition in a file.
    Either the condition is static (known at compile-time), or dynamic.
    Each variant is available with True and False for exhaustive tests.
    """

    ST = "static_true"
    SF = "static_false"
    DT = "dynamic_true"
    DF = "dynamic_false"

    def __str__(self) -> str:
        return self.value


class CheckpointBuilder:
    """
    This class factorizes the code for producing checkpoints from a function
    with a decision whose staticness may be set depending on the test needs.
    Sub-classes implement the specific cases and are attached to a specific
    project that lives in a src-* folder next to this file.
    """

    _MAIN = "main"

    EXPECT_EMPTY: ClassVar[CovReport] = {}
    EXPECT_VIOLATION: ClassVar[CovReport] = {}

    def __init__(self, cov_level: str):
        self.cov_level = cov_level

        # Instantiate the right source directory
        self._copy_src_dir()

    @abc.abstractproperty
    def src_dir(self) -> str:
        pass

    def _copy_src_dir(self) -> None:
        """
        Copy the test-case's directory (which is proper to the checkpoint
        class) into the test's current dir
        """
        src_src = self.src_dir
        src_dst = os.path.abspath("src")
        if os.path.exists(src_dst):
            shutil.rmtree(src_dst)
        full_src_src = os.path.abspath(
            os.path.join(os.path.dirname(__file__), "common", src_src)
        )
        shutil.copytree(full_src_src, src_dst)

    def make_checkpoint(
        self,
        name: str,
        xvars: list[tuple[str, str]],
        deps: list[str] | None = None,
    ) -> str:
        """
        Function in charge of compiling the project with a given combination
        of PkgTypes for each condition, and creating a checkpoint from the
        program execution.
        """
        gpr_obj_dir = "obj"

        prj_file = gprfor(
            mains=[f"{self._MAIN}.adb"],
            prjid=name,
            srcdirs=os.path.join("src"),
            objdir=gpr_obj_dir,
            deps=[os.path.join("src", "greet.gpr")] + (deps or []),
        )

        ckpt = f"test_{name}.ckpt"

        build_run_and_coverage(
            gprsw=GPRswitches(
                root_project=prj_file,
                units=["greet"],
                xvars=xvars,
            ),
            covlevel=self.cov_level,
            mains=[self._MAIN],
            gpr_obj_dir=gpr_obj_dir,
            extra_instr_args=["-v"],
            extra_coverage_args=[
                "-v",
                "--units=greet",
                f"--save-checkpoint={ckpt}",
            ],
        )
        return ckpt


class SimpleConditionCheckpointBuilder(CheckpointBuilder):

    EXPECT_EMPTY = {"greet.adb.xcov": {}, "greet.ads.xcov": {}}
    EXPECT_VIOLATION = {"greet.adb.xcov": {"!": {9}}, "greet.ads.xcov": {}}

    @property
    def src_dir(self) -> str:
        return "src-simple"

    def new(self, dec_type: PkgType) -> str:
        return self.make_checkpoint(
            str(dec_type), xvars=[("DECISION_TYPE", str(dec_type))]
        )


class IfStmtCheckpointBuilder(CheckpointBuilder):
    EXPECT_EMPTY_STMT_COVERED: ClassVar[CovReport] = {
        "greet.adb.xcov": {},
        "greet.ads.xcov": {},
    }
    EXPECT_EMPTY_STMT_NOT_COVERED: ClassVar[CovReport] = {
        "greet.adb.xcov": {"-": {9}},
        "greet.ads.xcov": {},
    }
    EXPECT_VIOLATION_STMT_COVERED: ClassVar[CovReport] = {
        "greet.adb.xcov": {"!": {8}},
        "greet.ads.xcov": {},
    }
    EXPECT_VIOLATION_STMT_NOT_COVERED: ClassVar[CovReport] = {
        "greet.adb.xcov": {"!": {8}, "-": {9}},
        "greet.ads.xcov": {},
    }

    @property
    def src_dir(self) -> str:
        return "src-if-stmt"

    def new(self, dec_type: PkgType) -> str:
        return self.make_checkpoint(
            str(dec_type), xvars=[("DECISION_TYPE", str(dec_type))]
        )


class DoubleConditionCheckpointBuilder(CheckpointBuilder):
    EXPECT_EMPTY = {"greet.adb.xcov": {}, "greet.ads.xcov": {}}
    EXPECT_VIOLATION = {"greet.adb.xcov": {"!": {11}}, "greet.ads.xcov": {}}

    @property
    def src_dir(self) -> str:
        return "src-double"

    def new(self, type1: PkgType, type2: PkgType) -> str:
        name = f"{type1}_{type2}"
        return self.make_checkpoint(
            name, xvars=[("DEC1", str(type1)), ("DEC2", str(type2))]
        )


class TestCaseRunner:
    """
    This class eases the verification of checkpoint consolidation results
    through the xcov report
    """

    def __init__(self, cov_level: str):
        self.base_cov_args = [
            "coverage",
            "--annotate=xcov",
            f"--level={cov_level}",
        ]

    def run_test_case(
        self,
        name: str,
        checkpoints: list[str],
        expected_report: CovReport,
        verbose: bool = True,
    ) -> None:
        thistest.log(f"Test Case {name}")
        slug_name = name.lower().replace(" ", "_")

        output_dir = f"testcase-{slug_name}"

        args = self.base_cov_args.copy()
        args += [f"--output-dir={output_dir}"]

        if verbose:
            args += ["-v"]

        # Add checkpoints
        args += [item for ckpt in checkpoints for item in ["-C", ckpt]]

        xcov(args, f"{slug_name}.log")

        check_xcov_reports(output_dir, expected_report)
