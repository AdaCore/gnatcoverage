"""
Check that "gnatcov instrument" produces the expected debug logging.
"""

from __future__ import annotations

import glob
import os.path
import re

from e3.fs import mkdir, sync_tree
from e3.testsuite.driver.diff import OutputRefiner, Substitute

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import xcov


# Copy projects and their sources in a temporary directory
tmp = Wdir("tmp_")
for pattern in ["*.gpr", "src-*"]:
    for item in glob.glob(os.path.join("..", pattern)):
        sync_tree(item, os.path.basename(item))

# Create an instrumentation output directories to exercise
# INSTRUMENT_CLEAN_OBJDIRS variations.
for project in ["mylib", "harness"]:
    mkdir(os.path.join("obj", project, f"{project}-gnatcov-instr"))


class HashRefiner(OutputRefiner):
    """Output refiner to hide hashes from the instrumented files.

    Actual hashes are not portable as they vary depending on the absolute path
    for original source files.
    """

    hash_re = re.compile("z([0-9a-f]{8})")

    def __init__(self):
        self.map = {}

    def repl(self, match):
        hash_value = match.group(0)
        try:
            return self.map[hash_value]
        except KeyError:
            result = f"[HASH#{len(self.map) + 1}]"
            self.map[hash_value] = result
            return result

    def refine(self, output):
        return self.hash_re.sub(self.repl, output)


class SortByFiles(OutputRefiner):
    """Output refiner to sort lines by instrumented source file.

    The order in which "gnatcov instrument" processes units is not specified,
    and varies in practice. This refiner reorders logs to have stable
    baselines.
    """

    def refine(self, output):
        result = []

        # When processing a section that contains logs for a list of source
        # files ("Coverage instrumentation" or "Main instrumentation"),
        # "by_file" maps seen filenames to the corresponding list of log lines
        # while "current_file" designates the last file seen for that section.
        by_file = None
        current_file = None

        def flush_files():
            """Append all log lines for files seen so far to result."""
            if by_file is None:
                return
            for _, lines in sorted(by_file.items()):
                result.extend(lines)

        for line in output.splitlines():
            if line in ("Coverage instrumentation", "Main instrumentation"):
                # This line starts a new section: flush log lines for the
                # previous section (if any) and reset "by_file" and
                # "current_file" to be ready for the new section.
                flush_files()
                by_file = {}
                current_file = None
                result.append(line)
            elif line.startswith(
                "[GNATCOV.INSTRUMENT_CLEAN_OBJDIRS] Processing "
            ):
                _, project_name = line.rsplit(None, 1)
                if by_file is None:
                    flush_files()
                    by_file = {}
                by_file[project_name] = [line]
                current_file = project_name
            elif by_file is None:
                # No section being processed: just forward this log line
                result.append(line)
            else:
                # If this line marks the start of processing for a new source
                # file, set "current_file" accordingly and start a new log line
                # for it.
                chunks = line.split()
                if len(chunks) == 2 and chunks[0] in ("[Ada]", "[C]", "[C++]"):
                    current_file = chunks[1]
                    assert current_file not in by_file
                    by_file[current_file] = [line]
                else:
                    by_file[current_file].append(line)
        flush_files()

        return "".join(f"{line}\n" for line in result)


# Run "gnatcov instrument" and check its output against our logging baselines
# for all example projects.
for project, subproject in [("tests", "mylib"), ("my_tool", "my_tool")]:
    log = f"instr-{project}.txt"
    xcov(
        [
            "instrument",
            f"-P{project}.gpr",
            "-cstmt",
            f"--projects={subproject}",
            "--log=instrument_clean_objdirs",
            "--log=instrument_sources",
        ],
        out=log,
    )
    thistest.fail_if_diff(
        baseline_file=f"../baseline-{project}.txt",
        actual_file=log,
        failure_message=f'[{project}] "gnatcov instrument" output',
        # To have portable baseline comparison, hide the actual CWD and
        # canonicalize directory separators.
        output_refiners=[
            Substitute(os.getcwd(), "[TMP]"),
            SortByFiles(),
            HashRefiner(),
            Substitute("\\", "/"),
        ],
        ignore_white_chars=False,
    )

thistest.result()
