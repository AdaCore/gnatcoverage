"""Helpers to test instrumentation on external SPARK codebases."""

from __future__ import annotations

import abc
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
        wd = Wdir("tmp_")

        adc_files = [adc_file, *self.extra_adc]
        mains = self.mains

        # Instrument, build and run the input project
        xcov_args = build_and_run(
            gprsw=self.create_gprsw(),
            covlevel="stmt+mcdc",
            mains=self.mains,
            extra_coverage_args=["--annotate=xcov"],
            extra_gprbuild_args=[
                f"-gnatec={filename}" for filename in adc_files
            ],
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
