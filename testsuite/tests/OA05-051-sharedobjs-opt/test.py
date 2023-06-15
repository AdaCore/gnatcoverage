"""Test validation of --shared-object command-line option usage."""

import re

from SUITE.cutils import Wdir, indent
from SUITE.tutils import exepath_to, gprbuild, gprfor, thistest, xrun


wdir = Wdir('tmp_')


gprbuild(gprfor(['foo.adb'], srcdirs='..'))
exe = exepath_to('foo')

for opt, err_msg in [
    ('none', None),
    ('all', None),
    ('foo.so', None),
    ('foo.so bar.so', None),

    ('none all', '"none" and "all" cannot be used at the same time'),
    ('all none', '"none" and "all" cannot be used at the same time'),

    ('none foo.so', 'cannot provide shared objects with "none"'),
    ('foo.so none', 'cannot provide shared objects with "none"'),

    ('all foo.so', 'cannot provide shared objects with "all"'),
    ('foo.so all', 'cannot provide shared objects with "all"'),
]:
    args = []
    for arg in opt.split():
        args.extend(['-L', arg])
    args.append(exe)
    p = xrun(args, register_failure=err_msg is None)
    if err_msg:
        pattern = r'.*gnatcov.*: --shared-object\|-L: {}'.format(
            re.escape(err_msg))
        thistest.fail_if(
            p.status == 0,
            '"{}" was expected to fail, but it did not'.format(' '.join(args)))
        thistest.fail_if(
            not re.match(pattern, p.out.strip()),
            '\nError mesage:'
            '\n{}'
            '\nDoes not match the expected pattern:'
            '\n{}'.format(indent(p.out), indent(pattern)))

thistest.result()
