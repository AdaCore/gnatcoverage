This directory contains a copy of a subset of some tests related to exemptions,
and buffer annotations. The modifications compared to the original tests is
that the annotations in sources (pragmas and comments) have been neutralized,
and the test.py scripts have been augmented to include invocations to the
gnatcov add-annotation command to generate equivalent annotations.

The test results are supposed to be equivalent.

The purpose of these tests is to ensure that using external annotation file and
the "gnatcov add-annotation" command can be used in place of in-source
annotations.


Test directory names may have been shortened to avoid path limit issues in
Windows configs.

Ada pragmas for exemptions and coverage disabling are replaced by an annotate
pragma for a fictional tool so that the pragmas still generates a SCO

C/C++ exemption comments are simply emptied.

Buffer dump comments / pragmas are entirely removed.
