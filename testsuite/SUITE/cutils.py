"""Common utility functions.

This module exposes common utility functions, for both individual tests and the
toplevel suite driver. In particular, they don't depend on the current
"thistest" instance.
"""

import os.path
import re
import shutil
import sys
import tempfile

from e3.fs import cp, mkdir
from e3.os.fs import cd, which
from e3.os.process import Run


def unhandled_exception_in(log):
    """
    Whether the provided execution log contains an indication
    of a unhandled exception occurrence.
    """

    # Account for patterns emitted by either a regular runtime or
    # our custom last_chance_handlers.
    return re.search(
        pattern=(
            r"(!!! EXCEPTION RAISED !!!"
            r"|"
            r"raised [A-Z_]+ : [-._a-zA-Z]+:[0-9]+ \w+)"
        ),
        string=log,
    )


def strip_prefix(prefix, string):
    """
    If STRING starts with PREFIX, return STRING without the PREFIX part.
    Return the string unchanged otherwise.
    """
    return string[len(prefix) :] if string.startswith(prefix) else string


def no_ext(filename):
    """Return the filename with the extension stripped away."""
    return os.path.splitext(filename)[0]


def ext(filename):
    """Return the filename extension"""
    return os.path.splitext(filename)[1]


def exists(filename):
    """Return true if the filename exists"""
    return os.path.exists(filename)


def contents_of(filename):
    """Return contents of file FILENAME"""
    with open(filename) as fd:
        return fd.read()


def lines_of(filename):
    """Return contents of file FILENAME as a list of lines without the
    newline termination character."""
    with open(filename) as fd:
        return fd.read().splitlines()


def set_from(filename):
    """Return contents of file FILENAME as a set of words."""
    with open(filename) as fd:
        return set(fd.read().split())


def to_list(blob):
    """
    Turn input BLOB into a list if it isn't already. Handle None and whitespace
    separated strings. Return empty list otherwise.
    """
    return (
        list(blob)
        if isinstance(blob, (list, tuple))
        else blob.split()
        if isinstance(blob, str)
        else []
    )


def indent(blob, indent="  "):
    """
    Prefix each line in BLOB's with INDENT. BLOB can be either a single string
    or a list of strings. The result is a single string anyway.
    """
    lines = list(blob) if isinstance(blob, list) else blob.splitlines()
    return "\n".join("{}{}".format(indent, line) for line in lines)


def indent_after_first_line(blob, prefix="  "):
    """Like "indent", but do not change the first line."""
    lines = list(blob) if isinstance(blob, list) else blob.splitlines()
    if len(lines) < 2:
        return "\n".join(lines)
    else:
        return "\n".join(lines[0:1] + indent(lines[1:], prefix).splitlines())


def text_to_file(text, filename="tmp.list"):
    """
    Write TEXT to file FILENAME. Overwrite current contents. Return FILENAME.
    """
    with open(filename, "w") as fd:
        fd.write(text)
    return filename


def list_to_file(lines, filename="tmp.list"):
    """
    Write list LINES to file FILENAME, one item per line. Typical use is to
    generate response files. Return FILENAME.
    """
    return text_to_file("\n".join(lines) + "\n", filename)


def list_to_tmp(lines, dirname=None):
    """
    Write list LINES to a temporary file in DIRNAME (or the current directory),
    one item per line. Return the temporary file name, chosen not to conflict
    with already exisiting files.
    """
    # By default, create the temporary file in the current working directory.
    # Make sure the returned path is absolute, so that the result can be used
    # even after the current working directory changes (which happens often in
    # testcases).
    dirname = os.getcwd() if dirname is None else os.path.abspath(dirname)
    return text_to_file(
        "\n".join(lines) + "\n",
        tempfile.mktemp(dir=dirname, suffix=".list"),
    )


def match(pattern, filename, flags=0):
    """Whether regular expression PATTERN could be found in FILENAME"""
    return re.search(pattern, contents_of(filename), flags) is not None


def re_filter(strings, pattern=""):
    """Compute the list of entries in STRINGS that match the PATTERN regexp."""
    return [t for t in strings if re.search(pattern, t)]


def clear(f):
    """Remove file F if it exists"""
    if os.path.exists(f):
        os.remove(f)


def empty(f):
    """True iif file F is empty, assumed to exist"""
    return os.stat(f).st_size == 0


def version(tool, nlines=1):
    """
    Return version information as reported by the execution of TOOL --version,
    expected on the first NLINES of output. If TOOL is not available from PATH,
    return a version text indicating unavailability.  If TOOL is 'gcc', append
    the target for which it was configured to the base version info.
    """

    # If TOOL is not on PATH, return a version text indicating unavailability.
    # This situation is legitimate here for gnatemu when running through a
    # probe, and if we happen to actually need the tool later on, we'll see
    # test failures anyway.
    if not which(tool):
        return "N/A"

    # --version often dumps more than the version number on a line. A
    # copyright notice is typically found there as well. Our heuristic
    # here is to strip everything past the first comma.

    def version_on_line(text):
        cprpos = text.find(",")
        return text[0:cprpos] if cprpos != -1 else text

    tool_version_output = [
        line.strip() for line in Run([tool, "--version"]).out.split("\n")
    ]
    version_info = "\n".join(
        [version_on_line(line) for line in tool_version_output[0:nlines]]
    )

    if tool == "gcc":
        gcc_target = Run([tool, "-dumpmachine"]).out.strip()
        version_info += " [%s]" % gcc_target

    return version_info


def ndirs_in(path):
    """Return the number of directory name components in PATH."""
    # Count how many times we can split PATH with os.path until reaching an
    # empty head. This lets os.path deal with the separator recognition
    nsplits = 0
    while path:
        path, tail = os.path.split(path)
        nsplits += 1
    return nsplits


def output_of(cmd, dirname=None):
    """
    Execute CMD and return it's output, switching to DIRNAME before if not
    None, and switching back to the original cwd as needed.
    """
    cwd = os.getcwd()
    if dirname is not None:
        cd(dirname)
    output = Run(cmd.split()).out
    cd(cwd)
    return output


class Wdir:
    """
    Simple helper to handle working directories.
    """

    def __init__(self, subdir=None):
        """
        If `subdir` is passed, create a subdirectory with this name and move
        there.
        """
        self.homedir = os.getcwd()
        if subdir:
            self.to_subdir(subdir)

    def to_subdir(self, dirname):
        """
        Change the current directory to `dirname`, relative to `self`'s home
        directory. Create it if needed, after first removing it.
        """
        self.to_homedir()
        if os.path.exists(dirname):
            shutil.rmtree(dirname)
        mkdir(dirname)
        cd(dirname)

    def to_homedir(self):
        """
        Change the current directory to `self`'s home directory.
        """
        cd(self.homedir)


class FatalError(Exception):
    """Exception to raise when processing has to stop."""

    def __init__(self, comment, outfile=None, outstr=None):
        if outfile is not None:
            comment += ". Output was:\n" + contents_of(outfile)
        elif outstr is not None:
            comment += ". Output was:\n" + outstr
        self.comment = comment

    def __str__(self):
        return self.comment


def exit_if(t, comment):
    """
    If `t` is true, print `comment` on the standard error stream and exit with
    error status code.
    """
    if t:
        sys.stderr.write(comment + "\n")
        exit(1)


class Identifier:
    def __init__(self, name):
        self.name = name


def copy_to_dir(orig_dir, target_dir, filename):
    """
    Copy filename from orig_dir to target_dir.
    """
    if os.path.abspath(orig_dir) != os.path.abspath(target_dir):
        cp(
            os.path.join(orig_dir, filename),
            os.path.join(target_dir, filename),
        )
