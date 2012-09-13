# ****************************************************************************
# **                         TestCase Control abstractions                  **
# ****************************************************************************

# =======================
# == Testcase Category ==
# =======================

# A specific category tells the testsuite engine which coverage notes are
# relevant for a testcase execution.

class _Category:
    def __init__(self, name):
        self.name = name

# This is the set of valid categories. Note that some testcases such as
# those checking the conformance of the output report format, pertain to no
# category in particular.

class CAT:

    # Set of criteria related categories

    stmt = _Category (name = "stmt")
    decision = _Category (name = "decision")
    mcdc = _Category (name = "mcdc")

    critcats = (stmt, decision, mcdc)

    # Special value asking to determine the category from the Testcase
    # location (relative pathname from the testsuite root)

    auto = _Category (name = "auto")

