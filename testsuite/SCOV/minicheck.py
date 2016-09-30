import collections
import glob
import re

from SUITE.cutils import FatalError, contents_of
from SUITE.tutils import thistest, xcov

"""
Dummy XCOV reports checker.

This is a temporary module to help testcases to check coverage analysis
reports. Ideally, we should use the regular SCOV circuitry for that but that's
not possible at the moment for manual report production schemes (for instance
for specific checkpoints usage testcases).
"""

COV_RE = re.compile('^ *(\d+) (.):.*$')


def checked_xcov(args, out_file):
    """
    Run "xcov" and make the testcase fail if the output file is not empty.
    """
    xcov(args, out_file)
    out = contents_of(out_file)
    thistest.fail_if(
        out,
        'gnatcov output not empty ({}):\n'
        '   {}\n'
        '{}'.format(out_file, ' '.join(args), out)
    )


def fmt_cov(cov_data):
    """
    Format coverage data into a human readable form.

    This can be used to report differences between expected/got coverage
    reports.
    """
    result = []
    for cov_char in sorted(cov_data):
        result.append('{}({})'.format(
            cov_char, ', '.join(str(lineno)
                                for lineno in sorted(cov_data[cov_char]))
        ))
    return ' '.join(result)


def check_xcov_content(filename, expected_cov):
    """
    Dumbed-down version of coverage matching. Check that the XCOV file
    "filename" matches some expected coverage data.

    "expected_cov" is a dict like:

    >>> {'+': {5, 7},
         '!': {6}}

    This is interpreted as: lines 5 and 7 must be fully covered (+), line 6
    must be partially covered (!) and all other lines must be no-code (.).
    """
    got_cov = collections.defaultdict(set)
    with open(filename) as f:
        for line in f:
            m = COV_RE.match(line)
            if m:
                lineno, cov_char = m.groups()
                if cov_char != '.':
                    got_cov[cov_char].add(int(lineno))

    thistest.fail_if(
        got_cov != expected_cov,
        '{}: unexpected coverage report content:\n'
        'Expected: {}\n'
        'But got:  {}\n'.format(
            filename, fmt_cov(expected_cov), fmt_cov(got_cov)
        )
    )


def check_xcov_reports(xcov_filename_pattern, expected_cov):
    """
    Check the set of XCOV report files and their content.

    Collect files that match "xcov_filename_pattern" (a glob pattern) and check
    the set of files matches "expected_cov". Then, check that each report
    matches the expected coverage results.

    "expected_cov" is a mapping: filename -> coverage data. See
    "check_xcov_content" for the coverage data format.
    """

    def fmt_sorted_indented_list(items):
        return '\n'.join('  {}'.format(s) for s in sorted(items))

    xcov_files = {f for f in glob.glob(xcov_filename_pattern)}
    thistest.stop_if(
        xcov_files != set(expected_cov),
        FatalError(
            'Unexpected XCOV files. Expected:\n'
            '{}\n'
            'But got instead:\n'
            '{}\n'.format(fmt_sorted_indented_list(expected_cov),
                          fmt_sorted_indented_list(xcov_files))
        )
    )

    for filename, cov_data in expected_cov.items():
        check_xcov_content(filename, cov_data)
