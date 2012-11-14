# ****************************************************************************
# **                              NOTE EXPANDERS                            **
# ****************************************************************************

# Note expansion is the process that consists in transforming expectations or
# coverage indications in text form into internal representations that we can
# manipulate

# Our facilities construct per-unit dictionaries of expected or reported
# coverage notes (Enote objects) extracted from text files (=xcov outputs,
# =report outputs, or driver sources).

# The result dictionary keys are source names and values are KnoteDict
# objects (per kind dictionary of note instances)
#
# { [sourcename] -> { [note kind] -> [ Cnote, Cnote, ... ],
#                     ...
#                   },
#   ...
# }

# Below is a rough sketch of the entities and classes involved:
#
#   test_blob1.adb       blob1.adb
#   ...                      v
#   Expect.Patterns   >  XnotesExpander
#     (XnoteP)               v
#                 .xlnotes = { sourcename - KnoteDict(lNoteKinds) of Xnote }
#                 .xrnotes = { sourcename - KnoteDict(rNoteKinds) of Xnote }
#
# xcov --annotate=xcov   -> *.xcov
#                            v LnotesExpander
#                  .elnotes = { sourcename - KnoteDict(lNoteKinds) of Enote }
#
# xcov --annotate=report -> test.rep
#                            v RnotesExpander
#                  .ernotes = { sourcename - KnoteDict(rNoteKinds) of Enote }

# The main facilities (XnotesExpander, RnotesExpander and LnotesExpander) are
# implemented in separate modules (x|r|lnexpanders.py)

# ****************************************************************************

