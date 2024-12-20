#! /usr/bin/env python

import argparse
import os.path
import sys

try:
    from colorama import Fore, Style

    CYAN = Fore.CYAN
    GREEN = Fore.GREEN
    MAGENTA = Fore.MAGENTA
    RESET_ALL = Style.RESET_ALL
except ImportError:
    MAGENTA = "\033[35m"
    CYAN = "\033[36m"
    GREEN = "\033[32m"
    RESET_ALL = "\033[0m"


args_parser = argparse.ArgumentParser(
    description="Check basic formatting rules on text files"
)
args_parser.add_argument(
    "--autofix",
    action="store_true",
    help="When possible, update files to fix detected formatting issues",
)
args_parser.add_argument(
    "--force-colors",
    action="store_true",
    help=(
        "Force colored output. This is necessary when the output is not a TTY"
        " but still supports ANSI codes."
    ),
)
args_parser.add_argument(
    "--ignore-non-source",
    action="store_true",
    help=(
        "If set, filter the given files to only keep those which would have"
        " been detected automatically"
    ),
)
args_parser.add_argument(
    "files",
    nargs="*",
    help=(
        "List of files on which to run. If none is passed, process all source"
        " files found in the current directory that look like source files."
    ),
)


class Checker:
    filename_extensions = {
        "adb",
        "adc",
        "ads",
        "c",
        "cc",
        "cpp",
        "gpr",
        "h",
        "hh",
        "hpp",
        "opt",
        "py",
    }
    """
    List of file extensions that look like source files.
    """

    ignore_list = [
        "testsuite/tests/354-source-encoding/latin1.ads",
    ]
    """
    List of files to intentionally ignore for style checking purposes.
    """

    def __init__(self, autofix: bool = False, force_colors: bool = False):
        self.issue_found = False
        """
        Whether at least one issue was found so far.
        """

        self.use_colors = force_colors or os.isatty(sys.stdout.fileno())
        """
        Whether to colorize the error messages.
        """

        self.autofix = autofix
        """
        Whether to attempt to fix the style issues found in place.
        """

    def report(
        self,
        filename: str,
        message: str,
        lineno: int | None = None,
    ) -> None:
        """
        Report a style issue.

        :param filename: File in which the issue was found.
        :param message: Human-readable description of the issue.
        :param lineno: Line number for the issue, if applicable, None
            otherwise.
        """
        self.issue_found = True

        # Build a list of text chunks to print. Put colorama elements in tuples
        # so that we can keep only text chunks if the output is not a TTY.
        chunks = [
            (MAGENTA,),
            filename,
            (CYAN,),
            ":",
        ]
        if lineno is not None:
            chunks += [
                (GREEN,),
                str(lineno),
                (CYAN,),
                ":",
            ]
        chunks += [
            (RESET_ALL,),
            " ",
            message,
        ]

        filtered_chunks = []
        for c in chunks:
            if isinstance(c, str):
                filtered_chunks.append(c)
            elif isinstance(c, tuple):
                if self.use_colors:
                    filtered_chunks += c
            else:
                raise AssertionError
        print("".join(filtered_chunks))

    def process_file(self, filename: str) -> None:
        """
        Look for style issues in the given file.
        """

        with open(filename, encoding="utf-8") as f:
            try:
                content = f.read()
            except UnicodeDecodeError as exc:
                self.report(filename, str(exc))
                return
            if not content:
                return

            lines = content.splitlines()
            modified = False

            if not content.endswith("\n"):
                modified = True
                self.report(filename, "missing trailing newline")

            for i, line in enumerate(lines, 1):
                stripped_line = line.rstrip()
                if line != stripped_line:
                    modified = True
                    self.report(filename, "trailing whitespace", i)
                    lines[i - 1] = stripped_line

            while not lines[-1].strip():
                modified = True
                self.report(filename, "last line is empty")
                lines.pop()

            # If requested, fix the issues that were found
            if self.autofix and modified:
                with open(filename, "w") as f:
                    if lines:
                        for line in lines:
                            f.write(line)
                            f.write("\n")

    @classmethod
    def main(cls, argv: list[str] | None = None):
        args = args_parser.parse_args(argv)
        checker = cls(
            autofix=args.autofix,
            force_colors=args.force_colors,
        )

        ignore_set = {os.path.realpath(f) for f in checker.ignore_list}

        # Process the list of files to check if present, otherwise look for all
        # source files in the current directory.
        if args.files:
            for filename in args.files:
                realpath = os.path.realpath(filename)
                if not args.ignore_non_source or (
                    realpath not in ignore_set
                    and filename.rsplit(".", 1)[-1] in cls.filename_extensions
                ):
                    checker.process_file(filename)
        else:
            for path, _, filenames in os.walk("."):
                for f in filenames:
                    realpath = os.path.realpath(os.path.join(path, f))
                    if (
                        realpath not in ignore_set
                        and f.rsplit(".", 1)[-1] in cls.filename_extensions
                    ):
                        checker.process_file(realpath)

        return 1 if checker.issue_found else 0


if __name__ == "__main__":
    sys.exit(Checker.main())
