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

from gnatpython.ex import Run
from gnatpython.fileutils import cd, mkdir, which


def strip_prefix(prefix, string):
    """
    If STRING starts with PREFIX, return STRING without the PREFIX part.
    Return the string unchanged otherwise.
    """
    return string[len(prefix):] if string.startswith(prefix) else string


def no_ext(filename):
    """Return the filename with the extension stripped away."""
    return os.path.splitext(filename)[0]


def contents_of(filename):
    """Return contents of file FILENAME"""
    with open(filename) as fd:
        return fd.read()


def lines_of(filename):
    """Return contents of file FILENAME as a list of lines"""
    with open(filename) as fd:
        return fd.readlines()


def set_from(filename):
    """Return contents of file FILENAME as a set of words."""
    with open(filename) as fd:
        return set(fd.read().split())


def to_list(blob):
    """
    Turn input BLOB into a list if it isn't already. Handle None and whitespace
    separated strings. Return empty list otherwise.
    """
    return (list(blob) if isinstance(blob, (list, tuple))
            else blob.split() if isinstance(blob, str)
            else [])


def indent(blob, indent='  '):
    """
    Prefix each line in BLOB's with INDENT. BLOB can be either a single string
    or a list of strings. The result is a single string anyway.
    """
    lines = list(blob) if isinstance(blob, list) else blob.splitlines()
    return '\n'.join('{}{}'.format(indent, line)
                     for line in lines)


def indent_after_first_line(blob, prefix='  '):
    """Like "indent", but do not change the first line."""
    lines = list(blob) if isinstance(blob, list) else blob.splitlines()
    if len(lines) < 2:
        return '\n'.join(lines)
    else:
        return '\n'.join(lines[0:1] + indent(lines[1:], prefix).splitlines())


def text_to_file(text, filename='tmp.list'):
    """
    Write TEXT to file FILENAME. Overwrite current contents. Return FILENAME.
    """
    with open(filename, 'w') as fd:
        fd.write(text)
    return filename


def list_to_file(l, filename='tmp.list'):
    """
    Write list L to file FILENAME, one item per line. Typical use is to
    generate response files. Return FILENAME.
    """
    return text_to_file('\n'.join(l) + '\n', filename)


def list_to_tmp(l):
    """
    Write list L to a temporary file in the current directory, one item per
    line. Return the temporary file name, chosen not to conflict with already
    exisiting files.
    """
    # Creation in the current directory is of interest since this
    # is expected to take place from a temporary work dir cleaned up
    # afterwards, so most callers don't have to bother deleting the
    # temp files themselves.
    return text_to_file(
        '\n'.join(l) + '\n', tempfile.mktemp(dir=os.getcwd(), suffix='.list'))


def match(pattern, filename, flags=0):
    """Whether regular expression PATTERN could be found in FILENAME"""
    return re.search(pattern, contents_of(filename), flags) is not None


def re_filter(l, pattern=''):
    """Compute the list of entries in L that match the PATTERN regexp."""
    return [t for t in l if re.search(pattern, t)]


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
        return 'N/A'

    # --version often dumps more than the version number on a line. A
    # copyright notice is typically found there as well. Our heuristic
    # here is to strip everything past the first comma.

    def version_on_line(text):
        cprpos = text.find(',')
        return text[0:cprpos] if cprpos != -1 else text

    tool_version_output = Run([tool, '--version']).out.split('\n')
    version_info = '\n'.join(
        [version_on_line(l) for l in tool_version_output[0:nlines]])

    if tool == 'gcc':
        gcc_target = Run([tool, '-dumpmachine']).out.strip()
        version_info += ' [%s]' % gcc_target

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


def output_of(cmd, dir=None):
    """
    Execute CMD and return it's output, switching to DIR before if not None,
    and switching back to the original cwd as needed.
    """
    cwd = os.getcwd()
    if dir is not None:
        cd(dir)
    output = Run(cmd.split()).out
    cd(cwd)
    return output


class Wdir:
    """
    Simple helper to handle working directories.
    """

    def __init__(self, subdir=None, clean=False):
        """
        If `subdir` is passed, create a subdirectory with this name and move
        there. If `clean`, make sure it's removed first.
        """
        self.homedir = os.getcwd()
        if subdir:
            if clean and os.path.exists(subdir):
                shutil.rmtree(subdir)
            self.to_subdir(subdir)

    def to_subdir(self, dir):
        """
        Change the current directory to `dir` (a path relative to `self`'s home
        directory). Create it if needed.
        """
        self.to_homedir()
        mkdir(dir)
        cd(dir)

    def to_homedir(self):
        """
        Change the current directory to `self`'s home directory.
        """
        cd(self.homedir)


class FatalError(Exception):
    """Exception to raise when processing has to stop."""

    def __init__(self, comment, outfile=None, outstr=None):
        if outfile is not None:
            comment += '. Output was:\n' + contents_of(outfile)
        elif outstr is not None:
            comment += '. Output was:\n' + outstr
        self.comment = comment

    def __str__(self):
        return self.comment


def exit_if(t, comment):
    """
    If `t` is true, print `comment` on the standard error stream and exit with
    error status code.
    """
    if t:
        print >> sys.stderr, comment
        exit(1)


class Identifier:
    def __init__(self, name):
        self.name = name
