import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, indent
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


wd = Wdir('tmp_')
main = exepath_to('main')
main_trace = tracename_for('main')

gpr = gprfor(['main.c'], srcdirs='..', langs=('C', 'Asm'))
gprbuild(gpr)

xrun(main)
xcov(['coverage', '-cbranch', '-aasm', main_trace, '--routines=f'],
     out='coverage.log')

pattern = """\
Coverage level: branch
f !: [0-9a-f]+-[0-9a-f]+
[0-9a-f]+ \\+:  85 ff            test   %edi,%edi
[0-9a-f]+ v:  7e 1a            jle    0x[0-9a-f]+ <f\\+0x1e>
[0-9a-f]+ \\+:  31 c9            xor    %ecx,%ecx
[0-9a-f]+ \\+:  31 c0            xor    %eax,%eax
[0-9a-f]+ \\+:  0f 1f 84 00 00 00 00 00   nopl   0x0\\(%rax,%rax,1\\)
[0-9a-f]+ \\+:  89 ca            mov    %ecx,%edx
[0-9a-f]+ \\+:  21 fa            and    %edi,%edx
[0-9a-f]+ \\+:  01 d0            add    %edx,%eax
[0-9a-f]+ \\+:  ff c1            inc    %ecx
[0-9a-f]+ \\+:  39 cf            cmp    %ecx,%edi
[0-9a-f]+ v:  75 f4            jne    0x[0-9a-f]+ <f\\+0x10>
[0-9a-f]+ \\+:  eb 02            jmp    0x[0-9a-f]+ <f\\+0x20>
[0-9a-f]+ -:  31 c0            xor    %eax,%eax
[0-9a-f]+ \\+:  c3               ret
14 instructions analyzed:
  13 covered
  1 not executed
2 conditional branches analyzed:
  0 fully covered
  2 partially covered
  0 not executed"""

pattern_lines = pattern.splitlines()
log_lines = contents_of('coverage.log').splitlines()

thistest.fail_if(
    len(pattern_lines) != len(log_lines),
    'The output of "gnatcov coverage" is {} lines long, {} expected'.format(
        len(log_lines), len(pattern_lines)))

for pat, log in zip(pattern_lines, log_lines):
    thistest.fail_if(
        not re.match(pat, log),
        'Could not match "gnatcov coverage" output:\n'
        '{}\n'
        'against the expected pattern:\n'
        '{}\n'.format(indent(log), indent(pat)))

thistest.result()
