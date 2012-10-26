# ***************************************************************************
# **                         COMMON UTILITY functions                      **
# ***************************************************************************

# This module exposes common utility functions, for both individual tests and
# the toplevel suite driver. In particular, they don't depend on the current
# "thistest" instance.

# ***************************************************************************

import re, tempfile
from gnatpython.fileutils import diff, os, cd, mkdir
from gnatpython.ex import Run

# ------------
# -- no_ext --
# ------------
def no_ext(filename):
    """Return the filename with the extension stripped away."""
    return os.path.splitext(filename)[0]

# -----------------
# -- contents_of --
# -----------------
def contents_of(filename):
    """Return contents of file FILENAME"""
    with open(filename) as fd:
        contents = fd.read()
    return contents

# --------------
# -- lines_of --
# --------------
def lines_of(filename):
    """Return contents of file FILENAME as a list of lines"""
    with open(filename) as fd:
        contents = fd.readlines()
    return contents

# -------------
# -- to_list --
# -------------
def to_list(blob):
    """Turn input BLOB into a list if it isn't already. Handle None
       and whitespace separated strings. Return empty list otherwise."""

    return (
        blob if isinstance (blob, list)
        else blob.split() if isinstance (blob, str)
        else []
        )

# ------------------
# -- text_to_file --
# ------------------
def text_to_file(text, filename="tmp.list"):
    """Write TEXT to file FILENAME. Overwrite current contents.
    Return FILENAME."""

    with open (filename, "w") as fd:
        fd.write (text)
    return filename

# ------------------
# -- list_to_file --
# ------------------
def list_to_file(l, filename="tmp.list"):
    """Write list L to file FILENAME, one item per line. Typical use is
       to generate response files. Return FILENAME."""

    return text_to_file ('\n'.join (l) + '\n', filename)

# -----------------
# -- list_to_tmp --
# -----------------
def list_to_tmp(l):
    """Write list L to file FILENAME, one item per line. Typical use is
       to generate response files. Return FILENAME."""

    return text_to_file (
        '\n'.join (l) + '\n', tempfile.mktemp(suffix=".list"))

# -----------
# -- match --
# -----------
def match(pattern, filename, flags=0):
    """Whether regular expression PATTERN could be found in FILENAME"""
    return re.search(pattern, contents_of(filename), flags) is not None

# ---------------
# -- re_filter --
# ---------------
def re_filter(l, pattern=""):
    """Compute the list of entries in L that match the PATTERN regexp."""
    return [t for t in l if re.search(pattern,t)]

# -----------
# -- clear --
# -----------
def clear(f):
    """Remove file F if it exists"""
    if os.path.exists(f):
        os.remove(f)

# -----------
# -- empty --
# -----------
def empty(f):
    """True iif file F is empty, assumed to exist"""
    return os.stat(f).st_size == 0

# -------------
# -- version --
# -------------
def version(tool, nlines=1):
    """Return version information as reported by the execution of TOOL
    --version, expected on the first NLINES of output."""

    # --version often dumps more than the version number on a line. A
    # copyright notice is typically found there as well. Our heuristic
    # here is to strip everything past the first comma.

    def version_on_line (text):
        cprpos = text.find (",")
        return text [0:cprpos] if cprpos != -1 else text

    return '\n'.join (
        [version_on_line (l) for l in
         Run([tool, "--version"]).out.split('\n')[0:nlines]]
        )

# --------------
# -- ndirs_in --
# --------------
def ndirs_in(path):
    """Return the number of directory name components in PATH."""

    # Count how many times we can split PATH with os.path until reaching an
    # empty head. This lets os.path deal with the separator recognition

    nsplits = 0
    while path:
        (path, tail) = os.path.split(path)
        nsplits += 1

    return nsplits

# ==========
# == Wdir ==
# ==========

class Wdir:

    def __init__(self, subdir=None):
        self.homedir = os.getcwd()
        if subdir:
            self.to_subdir (subdir)

    def to_subdir (self, dir):
        self.to_homedir ()
        mkdir (dir)
        cd (dir)

    def to_homedir (self):
        cd (self.homedir)


# ==========================
# == FatalError Exception ==
# ==========================

# to raise when processing has to stop

class FatalError(Exception):
    def __init__(self,comment,output=None):
        if output != None:
            comment += '. Output was:\n'+contents_of(output)
        self.comment = comment

    def __str__(self):
        return self.comment

# =================
# == Identifiers ==
# =================

class Identifier:
    def __init__(self, name):
        self.name = name
