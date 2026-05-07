"""Helpers to test instrumentation on external SPARK codebases."""

from __future__ import annotations

import abc
import dataclasses
import os.path

from SCOV.minicheck import build_and_run
from SUITE.cutils import Wdir
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.tutils import xcov


# Absolute filename to the file that contains GNAT configuration pragmas that
# GNATcoverage users must apply when dealing with SPARK codebases.
adc_file = os.path.join(thistest.support_dir(), "instrument-spark.adc")


class TestCase:
    """Testcase to exercise instrumentation on an external SPARK codebase."""

    @abc.abstractmethod
    def create_gprsw(self) -> GPRswitches:
        """Return the GPR switches to pass to gnatcov/gprbuild.

        This method may create project files, if needed.
        """
        pass

    @property
    def mains(self) -> list[str]:
        """Return the list of mains to run after instrumentation.

        If this list is empty, no main will be run and thus no coverage report
        will be produced.
        """
        return []

    @property
    def extra_adc(self) -> list[str]:
        """Return the list of pragma configuration files to pass to gprbuild.

        This is in addition to "instrument-spark.adc" (distributed with
        gnatcov).
        """
        return []

    def run(self) -> None:
        """Run the testcase."""
        for params in GhostTestParams.all_params():
            wd = Wdir("tmp_" + params.slug)

            adc_files = [adc_file, *self.extra_adc]
            mains = self.mains
            gnatec_args = [f"-gnatec={filename}" for filename in adc_files]

            # Instrument, build and run the input project
            xcov_args = build_and_run(
                gprsw=self.create_gprsw(),
                covlevel="stmt+mcdc",
                mains=self.mains,
                extra_instr_args=params.extra_instr_args,
                extra_coverage_args=["--annotate=xcov"],
                extra_gprbuild_args=gnatec_args + params.extra_gprbuild_gargs,
                extra_gprbuild_cargs=["-cargs:Ada", "-gnat2022"],
            )

            # If there are mains, generate a coverage report from the traces
            if mains:
                xcov(xcov_args)

            self.analyze()

            wd.to_homedir()

    def analyze(self) -> None:
        """Subclasses can override this to perform additional checks.

        This runs after the coverage report has been produced (if there are
        mains) or after the instrumented project has been built (if there are
        no mains).
        """
        pass


@dataclasses.dataclass(frozen=True)
class GhostTestParams:
    """Helper to provide test parameters for ghost code instrumentation."""

    instrument_ghost: bool
    """Whether to instrument ghost code."""

    @property
    def slug(self) -> str:
        """Slug to create working directories."""
        return "ghost" if self.instrument_ghost else "noghost"

    @property
    def extra_instr_args(self) -> list[str]:
        """Extra arguments to pass to "gnatcov instrument"."""
        return ["--instrument-ghost"] if self.instrument_ghost else []

    @property
    def extra_gprbuild_gargs(self) -> list[str]:
        """Extra arguments to pass to gprbuild."""
        # Pass -gnata to gprbuild to enable assertion checking
        return ["-gnata"] if self.instrument_ghost else []

    @classmethod
    def all_params(cls) -> list[GhostTestParams]:
        """
        Return parameters to test with ghost code instrumentation and without.
        """
        return [GhostTestParams(False), GhostTestParams(True)]
