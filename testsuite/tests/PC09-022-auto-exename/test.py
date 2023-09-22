"""
Test that "gnatcov run" properly auto-detects the name of the executable from a
project file, or that it properly reports when there is not one single main.
"""

from collections import OrderedDict
import os
import os.path
import re

from SUITE.context import thistest
from SUITE.cutils import FatalError, Wdir, contents_of, indent
from SUITE.tutils import gprbuild, gprfor, xrun


wd = Wdir('tmp_')


class Project(object):
    def __init__(self, projects, name, srcdir, mains=(), deps=(), build=True,
                 should_succeed=True):
        self.projects = projects
        self.name = name
        self.srcdir = srcdir
        self.mains = list(mains)
        self.deps = deps
        self.build = build
        self.should_succeed = should_succeed

        self.objdir = 'obj-{}'.format(self.name)
        self.exedir = 'exe-{}'.format(self.name)

        self.prj_file = None

    @property
    def dep_projects(self):
        """
        Return the list of all projects "self" depends on.
        """
        return [self.projects[d] for d in self.deps]

    @property
    def all_mains(self):
        """
        Return the set of mains for this projects and all its dependencies'.
        """
        result = set()
        for main in self.mains:
            result.add(main.split('.')[0])
        for p in self.dep_projects:
            result.update(p.all_mains)
        return result

    def prepare(self):
        print('Preparing {}...'.format(self.name))
        self.prj_file = gprfor(
            mains=self.mains,
            prjid=self.name,
            srcdirs=os.path.join('..', self.srcdir),
            objdir=self.objdir,
            exedir=self.exedir,
            langs=['Ada'],
            deps=self.deps)
        if self.build:
            gprbuild(self.prj_file)

        # Make sure that in all case, output directories exist so that warning
        # messages don't pollute gnatcov's error messages.
        for dirname in (self.objdir, self.exedir):
            if not os.path.isdir(dirname):
                os.mkdir(dirname)

    def run(self):
        print('')
        print('== Running {} =='.format(self.name))

        error_msg_re = None
        all_mains = self.all_mains

        if len(all_mains) != 1:
            print('Not exactly one main: should fail')
            error_msg_re = (
                r'.*gnatcov.*: Please specify an executable to run \(EXE\)'
                r' on the command line.*')
        elif not self.build:
            main = all_mains.pop()
            print('Not built: should fail')
            error_msg_re = '.*gnatcov.*: .*{}.*: File not found'.format(
                re.escape(main))
        else:
            print('Should succeed')

        we_think_it_should_succeed = not error_msg_re
        thistest.stop_if(
            we_think_it_should_succeed != self.should_succeed,
            FatalError('Test declaration disagrees with test framework about'
                       ' whether this test should fail'))

        log_file = 'run-{}.txt'.format(self.name)
        p = xrun(['-P', self.prj_file],
                 register_failure=not error_msg_re,
                 out=log_file)
        if error_msg_re:
            thistest.fail_if(
                p.status == 0,
                'gnatcov run suceeded whereas it was expected to fail')
            out = contents_of(log_file).strip()
            thistest.fail_if(
                not re.match(error_msg_re, out),
                '\nFor {}, error message ({}):'
                '\n{}'
                '\nDoes not match the expected pattern:'
                '\n{}'
                .format(self.name, log_file,
                        indent(out), indent(error_msg_re)))


project_files = OrderedDict()
for p in (
    # Test that the most regular case (one project, one main) succeeds
    Project(project_files, 'single', 'src',
            mains=('main_proc.adb', ),
            should_succeed=True),

    # We expect an error when there is no main at all
    Project(project_files, 'nomain', 'src',
            should_succeed=False),

    # Likewise when there are too many mains
    Project(project_files, 'double', 'src',
            mains=('main_proc.adb', 'other_proc.adb'),
            should_succeed=False),

    # Likewise when there is one main but it has not been built
    Project(project_files, 'no_build', 'src',
            mains=('main_proc.adb', ),
            build=False,
            should_succeed=False),
):
    assert p.name not in project_files
    project_files[p.name] = p
    p.prepare()

for p in project_files.values():
    p.run()

thistest.result()
