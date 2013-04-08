# -*- coding: utf-8 -*-

import collections
import os
import os.path
import re

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import *


class TestCase(object):

    PROJECT_FILE  = '{}.gpr'
    ROUTINES_FILE = 'routines.list'
    RESULT_FILE   = 'coverage.result'

    SYMBOL_COVERAGE_PATTERN = re.compile(
        '^([a-zA-Z_][a-zA-Z0-9_]*)' # Symbol name
        ' ([-!+]): '                # Symbol coverage result
        '[0-9a-f]+-[0-9a-f]+\n$'    # Address range for the symbol
    )
    NO_COV, PART_COV, FULL_COV = '-!+'

    def __init__(self,
        test_drivers, coverage_expectations,
        extra_sourcedirs=[],
        level='branch', annotate='asm',
        extra_xcov_args=[],
    ):
        self.test_drivers = test_drivers
        self.coverage_expectations = coverage_expectations
        self.extra_sourcedirs = extra_sourcedirs
        self.level = level
        self.annotate = annotate
        self.extra_xcov_args = extra_xcov_args

    def run(self, register_failure=True):
        '''
        Return if "gnatcov coverage" executed properly.
        '''
        wd = Wdir('tmp_')

        # Compile and run separately each test driver.
        for test_driver, switches in self.test_drivers.items():
            self._compile(test_driver, *switches)
            self._run(test_driver)

        # Tell to gnatcov which symbols we are interested in (those are symbols
        # that appear in the coverage expectations).
        self._generate_routines_list()

        # Consolidate resulting traces and parse the object coverage results.
        # If consolidation fails, return False.
        if (
            not self._consolidate_traces(self.RESULT_FILE, register_failure)
            and not register_failure
        ):
            return False

        # We can parse the result only if the output is an annotated ASM.
        if self.annotate == 'asm':
            coverage_result = self._parse_coverage_results(self.RESULT_FILE)

            # Compare results with expectations...
            thistest.fail_if(
                coverage_result != self.coverage_expectations,
                'Coverage result:\n'
                '{}'
                'do not match coverage expectations:\n'
                '{}'.format(
                    self.format_coverage(coverage_result),
                    self.format_coverage(self.coverage_expectations)
                )
            )
        return True

    def _compile(self, test_driver, default_switches, compile_unit_switches):
        test_driver_wd = Wdir('{}-obj'.format(test_driver)).to_homedir()
        project_file = self.PROJECT_FILE.format(test_driver)

        with open(project_file, 'w') as f:
            f.write('''project {test_driver} is
           for Source_Dirs use ({sourcedirs});
           for Object_Dir  use "{test_driver}-obj";
           for Exec_Dir    use ".";
           for Languages   use ("C");
           for Main        use ("{test_driver}");

           package Compiler is
              for Default_Switches ("C")
                 use ({default_switches});'''.format(
                sourcedirs = self.fmt_list(['..'] + self.extra_sourcedirs),
                test_driver = test_driver,
                default_switches = self.fmt_list(default_switches)
            ))

            for compile_units, switches in compile_unit_switches:
                for cu in compile_units:
                    f.write('''
              for Switches ("{compile_unit}")
                 use Compiler'Default_Switches ("C") & ({switches});'''.format(
                        compile_unit = cu,
                        switches = self.fmt_list(switches)
                    ))

            f.write('''
           end Compiler;
        end {};
    '''.format(test_driver))

        gprbuild(project_file)

    def _run(self, test_driver):
        xrun(unixpath_to(test_driver))

    def _generate_routines_list(self):
        with open(self.ROUTINES_FILE, 'w') as f:
            for routine in self.coverage_expectations:
                f.write('{}\n'.format(routine))

    def _consolidate_traces(self, output, register_failure):
        xcov_args = [
            'coverage',
            '--level=' + self.level,
            '--annotate=' + self.annotate,
        ]
        if self.level in ('insn', 'branch'):
            xcov_args.append('--routines=@' + self.ROUTINES_FILE)
        xcov_args.extend(self.extra_xcov_args)
        xcov_args.extend(map(tracename_for, self.test_drivers))
        p = xcov(xcov_args, out=output, register_failure=register_failure)
        return p.status == 0

    def _parse_coverage_results(self, input_file):
        # Mapping: {symbol name -> {coverage status -> count} }
        result = collections.defaultdict(
            lambda: {self.NO_COV: 0, self.PART_COV: 0, self.FULL_COV: 0}
        )

        with open(input_file, 'r') as f:
            for line in f:
                m = self.SYMBOL_COVERAGE_PATTERN.match(line)
                if m:
                    symbol_name, coverage_status = m.groups()
                    result[symbol_name][coverage_status] += 1

        return result

    def fmt_list(self, items):
        '''
        Format a list of string for the GPR file.

        >>> fmt_list(('a', 'b', 'c'))
        "a", "b", "c"
        '''
        return ', '.join(['"{}"'.format(item) for item in items])

    def format_coverage(self, coverage):
        result = []
        for symbol in sorted(coverage):
            cov_result = coverage[symbol]
            result = '  - symbol "{}": {}-  {}!  {}+\n'.format(
                symbol,
                cov_result[self.NO_COV],
                cov_result[self.PART_COV],
                cov_result[self.FULL_COV]
            )
        return ''.join(result)
