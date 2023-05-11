"""
Testsuite qualification data facilities

This module exposes the testsuite qualification data management facilities,
aimed at the production of a test-results report post testsuite execution.

For each testcase:

* The testcase execution dumps a Qdata instance which holds details of what
  expectations the test had and how they were satisfied.

* The toplevel driver dumps a TC_status instance which holds the overall
  testcase execution status, accounting for the test success combined with
  test.opt related info.

The toplevel driver also dumps a SUITE_context instance which holds global
information about the testsuite run as a whole, such as the command line
options received etc. This is encapsulated as the SUITEdata class in this file.
"""

import os

from SUITE import dutils
from SUITE.cutils import output_of, version


# List of languages we support qualification tests for
QLANGUAGES = ["Ada"]

# String that identifies a qualification test at the beginning of it's
# sub-directory name relative to the testsuite root
QROOTDIR = "Qualif"

# Extension of datafiles dumped for the purpose of the STR document
# production
STREXT = ".dump"

# Name of a directory where context data aimed at the STR production engine
# will be dropped by the testsuite execution driver
QSTRBOX_DIR = "_strbox"


#
# SUITE_context facilities
#

# Helpers to construct the testsuite context data of relevance for STR
# production and consistency checking purposes.
#
# Context data is dumped by testsuite.py at the location where the testsuite is
# run. Part of this is read back from the same host/testsuite-dir context
# during the STR production.  Part of this is read back for consistency checks
# when producing a qualification kit. The latter might take place on a
# different host and using a different version of python so the context data
# needs to be produced in a format suitable for data exchange using persistent
# storage. To facilitate this, the relevant data items are all expressed using
# core string and dictionary python types.

# Name of a file, relative to the testsuite toplevel directory, where the
# testsuite data of use for the STR production will be made available.
CTXDATA_FILE = os.path.join(QSTRBOX_DIR, "suite" + STREXT)


def TOOL_info(exename, ver=None):
    """Context data for a tool involved in a testsuite run."""
    return {"exename": exename, "version": ver or version(exename)}


def OPT_info_from(options):
    """Context data for the command line options info of relevance."""
    return {
        "target": options.target,
        "board": options.board,
        "trace_mode": options.trace_mode,
        "dolevel": options.qualif_level,
        "cargs": options.cargs,
        "cargs_Ada": options.cargs_Ada,
        "cargs_C": options.cargs_C,
    }


def SUITE_context(
    treeref, runstamp, host, cmdline, options, gnatpro, gnatemu, gnatcov, other
):
    """
    Toplevel context data structure, wrapping up all the relevant items
    together.
    """
    return {
        "runstamp": runstamp,
        "treeref": treeref,
        "cmdline": cmdline,
        "options": options,
        "host": host,
        "gnatpro": gnatpro,
        "gnatemu": gnatemu,
        "gnatcov": gnatcov,
        "other": other,
    }


class TC_status:
    """Testcase execution status.

    Dumped for each testcase by the toplevel testsuite driver.
    """

    def __init__(self, passed=None, xfail=None, status=None, comment=None):
        self.passed = passed
        self.xfail = xfail
        self.status = status
        self.comment = comment


#
# Qualification Data classes, filled and dumped by testcases
#


class QDentry:
    def __init__(self, xfile, drivers, xrnotes, wdir):
        """
        :param xfile: Expectation file.
        :param drivers: Drivers run to satisfy them.
        :param xrnotes: Expected report notes, KnodeDict per source.
        :param wdir: Working directory where tc ran.
        """
        self.xfile = xfile
        self.drivers = drivers
        self.xrnotes = xrnotes
        self.wdir = wdir


class Qdata:
    """
    We expect one Qdata instance per qualification testcase, which hosts a
    sequence of QDentry instances; typically one per SCOV_helper execution
    (that is, per driver + consolidation spec).
    """

    def __init__(self, tcid):
        # These are filled and dumped by the testcase execution itself:
        self.entries = []
        self.tcid = tcid

    # Testcase interface

    def register(self, ob):
        self.entries.append(ob)

    def flush(self):
        dutils.pdump_to(qdaf_in("."), o=self)


QUALDATA_FILE = "tc" + STREXT


def qdaf_in(dirname):
    """
    Filename for qualification data to be pickled in DIR for a testcase.  This
    hosts instances of objects representing test executions, each holding
    dictionaries of expected notes together with their dischargers.
    """
    return os.path.join(dirname, QUALDATA_FILE)


def qdafs_from(dirname):
    """
    List of filenames for qualification data to be pickled, from the directory
    tree rooted at dirname. Each returned filename hosts instances of objects
    representing test executions, each holding dictionaries of expected notes
    together with their dischargers.
    """
    return [
        os.path.join(dir, file)
        for (dir, _, files) in os.walk(dirname)
        for file in files if file == QUALDATA_FILE
    ]


STATUSDATA_FILE = "tcs" + STREXT

def stdf_in(dirname):
    """Filename for execution status data to be picked up DIR"""
    return os.path.join(dirname, STATUSDATA_FILE)


def treeref_at(dirname):
    """
    A string representative of the git commit where the DIRNAME
    directory originates from, to be used for consistency checks
    when  multiple operations are done separately but should work
    over synchronized directory trees.
    """
    # Assuming git, sha1 for the HEAD reference
    return output_of("git rev-parse HEAD", dirname=dirname).rstrip("\n")
