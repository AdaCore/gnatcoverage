# ****************************************************************************
# **                TESTSUITE QUALIFICATION-DATA FACILITIES                 **
# ****************************************************************************

# This module exposes the testsuite qualification data management facilities,
# aimed at the production of a test-results report post testsuite execution.
#
# Testcases dump Qdata instances when they execute. The toplevel driver dumps
# the overall execution status, accounting for the test success combined with
# test.opt related info.

# ****************************************************************************

import os, sys, pickle, re

QLANGUAGES = ["Ada"]
# list of languages we support qualification tests for

QROOTDIR="Qualif"
# String that identifies a qualification test at the beginning of it's
# sub-directory name relative to the testsuite root

# -----------------
# -- STRbox_Data --
# -----------------

QSTRBOX_DIR="_strbox"
# Name of a directory where data aimed at the STR production engine will
# be dropped by the testsuite execution driver

QSTRBOX_FILE=os.path.join (QSTRBOX_DIR, "run.pkl")
# Name of a file, relative to the testsuite toplevel directory, where the
# testsuite data of use for the STR production will be made available.

class STRbox_Data:

    def __init__ (
        self, target=None,
        suite_cmdline=None, suite_options=None
        ):
        self.target = target
        self.suite_cmdline = suite_cmdline
        self.suite_options = suite_options

    def drop_to(self, filename):
        with open (filename, 'w') as f:
            pickle.dump (self, f)

# -------------
# -- qdaf_in --
# -------------

QUALDATA_FILE = "tc.pkl"

def qdaf_in(dir):
    """Filename for qualification data to be pickled in DIR for a testcase.
    This hosts instances of objects representing test executions, each holding
    dictionaries of expected notes together with their dischargers."""
    return os.path.join (dir, QUALDATA_FILE)

# -------------
# -- stdf_in --
# -------------

STATUSDATA_FILE = "tcs.pkl"

def stdf_in(dir):
    """Filename for execution status data to be picked up DIR"""
    return os.path.join (dir, STATUSDATA_FILE)

# ================================================================
# == Qualification Data classes, filled and dumped by testcases ==
# ================================================================

# We expect one Qdata instance per qualification testcase, which hosts a
# sequence of entries; typically one per SCOV_helper execution (that is, per
# driver + consolidation spec)

class QDentry:

    def __init__(self, xfile, drivers, xrnotes, wdir):
        self.xfile = xfile       # expectation file
        self.drivers = drivers   # drivers run to satisfy them
        self.xrnotes = xrnotes   # expected report notes, KnoteDict per source
        self.wdir    = wdir      # working directory where the tc ran

class Qdata:

    def __init__(self, tcid):

        # These are filled and dumped by the testcase execution itself:

        self.entries = []
        self.tcid = tcid

    # ------------------------
    # -- Testcase interface --
    # ------------------------

    def register(self, ob):
        self.entries.append (ob)

    def flush(self):
        with open(qdaf_in("."), "w") as fd:
            pickle.dump(self, fd)
