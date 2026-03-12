from __future__ import annotations

import collections
import re
from typing import TYPE_CHECKING

from e3.fs import mkdir

from SUITE.context import thistest
import SUITE.control
from SUITE.cutils import Wdir
from SUITE.tutils import (
    gprbuild,
    gprfor,
    tracename_for,
    unixpath_to,
    xcov,
    xrun,
)


if TYPE_CHECKING:
    CovData = dict[str, dict[str, int]]


# Cache some values we need repeatedly

TARGET_INFO = SUITE.control.target_info()


class TestCase:
    ROUTINES_FILE = "routines.list"
    RESULT_FILE = "coverage.result"

    SYMBOL_COVERAGE_PATTERN = re.compile(
        "^([a-zA-Z_][a-zA-Z0-9_]*)"  # Symbol name
        " ([-!+]): "  # Symbol coverage result
        "[0-9a-f]+-[0-9a-f]+\n$"  # Address range for the symbol
    )
    NO_COV = "-"
    PART_COV = "!"
    FULL_COV = "+"

    def __init__(
        self,
        test_drivers: dict[str, dict[str, list[str]]],
        coverage_expectations: CovData,
        extra_sourcedirs: list[str] | None = None,
        level: str = "branch",
        annotate: str = "asm",
        extra_xcov_args: list[str] | None = None,
    ):
        self.test_drivers = test_drivers
        self.coverage_expectations = {
            TARGET_INFO.to_platform_specific_symbol(symbol): data
            for symbol, data in coverage_expectations.items()
        }
        self.extra_sourcedirs = extra_sourcedirs or []
        self.level = level
        self.annotate = annotate
        self.extra_xcov_args = extra_xcov_args or []

    def run(self, register_failure: bool = True) -> bool:
        """
        Return if "gnatcov coverage" executed properly.
        """
        Wdir("tmp_")

        # Compile and run separately each test driver.
        for test_driver, switches in self.test_drivers.items():
            self._compile(test_driver, switches)
            self._run(test_driver)

        # Tell to gnatcov which symbols we are interested in (those are symbols
        # that appear in the coverage expectations).
        self._generate_routines_list()

        # Consolidate resulting traces and parse the object coverage results.
        # If consolidation fails, return False.
        if (
            not self._consolidate_traces(self.RESULT_FILE, register_failure)
            and not register_failure
        ):
            return False

        # We can parse the result only if the output is an annotated ASM.
        if self.annotate == "asm":
            coverage_result = self._parse_coverage_results(self.RESULT_FILE)

            # Compare results with expectations...
            thistest.fail_if(
                coverage_result != self.coverage_expectations,
                "Coverage result:\n"
                "{}"
                "do not match coverage expectations:\n"
                "{}".format(
                    self.format_coverage(coverage_result),
                    self.format_coverage(self.coverage_expectations),
                ),
            )
        return True

    def _compile(
        self,
        test_driver: str,
        compile_unit_switches: dict[str, list[str]],
    ) -> None:
        mkdir("{}-obj".format(test_driver))

        project_file = gprfor(
            mains=[test_driver + ".c"],
            prjid=test_driver,
            srcdirs=[".."] + self.extra_sourcedirs,
            objdir="{}-obj".format(test_driver),
            langs=["C", "ASM"],
            compiler_extra="\n".join(
                (
                    'for Switches("{}") use '
                    ' Compiler\'Default_Switches ("C") & ({});'
                ).format(cu, self.fmt_list(switches))
                for cu, switches in compile_unit_switches.items()
            ),
        )

        # We never want the testuite optimization options or source coverage
        # options to interfere with object coverage testcases as these are very
        # sensitive to code generation.
        gprbuild(project_file, scovcargs=False, suitecargs=False)

    def _run(self, test_driver: str) -> None:
        xrun(unixpath_to(test_driver))

    def _generate_routines_list(self) -> None:
        with open(self.ROUTINES_FILE, "w") as f:
            for routine in self.coverage_expectations:
                f.write("{}\n".format(routine))

    def _consolidate_traces(self, output: str, register_failure: bool) -> bool:
        xcov_args = [
            "coverage",
            "--level=" + self.level,
            "--annotate=" + self.annotate,
        ]
        if self.level in ("insn", "branch"):
            xcov_args.append("--routines=@" + self.ROUTINES_FILE)
        xcov_args.extend(self.extra_xcov_args)
        xcov_args.extend(map(tracename_for, self.test_drivers))
        p = xcov(xcov_args, out=output, register_failure=register_failure)
        return p.status == 0

    def _parse_coverage_results(self, input_file: str) -> CovData:
        # Mapping: {symbol name -> {coverage status -> count} }
        result: CovData = collections.defaultdict(
            lambda: {self.NO_COV: 0, self.PART_COV: 0, self.FULL_COV: 0}
        )

        with open(input_file, "r") as f:
            for line in f:
                m = self.SYMBOL_COVERAGE_PATTERN.match(line)
                if m:
                    symbol_name, coverage_status = m.groups()
                    result[symbol_name][coverage_status] += 1

        return result

    def fmt_list(self, items: list[str]) -> str:
        """
        Format a list of string for the GPR file.

        >>> fmt_list(('a', 'b', 'c'))
        "a", "b", "c"
        """
        return ", ".join(['"{}"'.format(item) for item in items])

    def format_coverage(self, coverage: CovData) -> str:
        result: list[str] = []
        for symbol in sorted(coverage):
            cov_result = coverage[symbol]
            result.append(
                '  - symbol "{}": {}-  {}!  {}+\n'.format(
                    symbol,
                    cov_result[self.NO_COV],
                    cov_result[self.PART_COV],
                    cov_result[self.FULL_COV],
                )
            )
        return "".join(result)
