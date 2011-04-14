# ***************************************************************************
# **                         COMMON UTILITY functions                      **
# ***************************************************************************

# This module exposes common utility functions, for both individual tests and
# the toplevel suite driver. In particular, they don't depend on the current
# "thistest" instance.

# ***************************************************************************

import re
from gnatpython.fileutils import diff

# -----------------
# -- contents_of --
# -----------------
def contents_of(filename):
    """Return contents of file FILENAME"""
    f = open(filename)
    contents = f.read()
    f.close()

    return contents

# --------------
# -- lines_of --
# --------------
def lines_of(filename):
    """Return contents of file FILENAME as a list of lines"""
    f = open(filename)
    contents = f.readlines()
    f.close()

    return contents

# -------------
# -- to_list --
# -------------
def to_list(blob):
    """Turn input BLOB into a list if it isn't already. Handle None
       and whitespace separated strings. Return empty list otherwise."""

    if isinstance (blob, list):
        return blob

    if blob == None:
        return []

    if isinstance (blob, str):
        return blob.split ()

    return []

# ------------------
# -- text_to_file --
# ------------------
def text_to_file(text, filename="tmp.list"):
    """Write TEXT to file FILENAME. Overwrite current contents.
    Return FILENAME."""

    f = open (filename, "w")
    f.write (text)
    f.close ()
    return filename

# ------------------
# -- list_to_file --
# ------------------
def list_to_file(l, filename="tmp.list"):
    """Write list L to file FILENAME, one item per line. Typical use is
       to generate response files. Return FILENAME."""

    return text_to_file ('\n'.join (l) + '\n', filename)

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

