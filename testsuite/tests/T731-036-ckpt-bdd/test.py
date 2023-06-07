"""
Regression testcase for the loading of BDD nodes from checkpoints.

When loading a checkpoint, gnatcov used to load the whole BDD table once per
loaded CU (whereas the BDD table contains all BDDs for all CUs). For some usage
patterns, this could even trigger a memory exhaustion in gnatcov. This testcase
checks that the BDD table does not grow when we load/save the same checkpoint
over and over.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_file, text_to_file
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

# Generate sources for the test program. The point of this program is to have
# one unit (DC_Unit) that generates lots of BDD nodes (i.e. lots of decisions),
# several other units (Simple_Unit_*), and a main that uses all these units.

# First emit a helper for DC_Unit
helper_ads_text = """
package Helper is
   function A return Boolean;
   function B return Boolean;
   function C return Boolean;
end Helper;
"""
helper_adb_text = """
package body Helper is
   function A return Boolean is
   begin
      return False;
   end A;
   function B return Boolean is
   begin
      return True;
   end B;
   function C return Boolean is
   begin
      return False;
   end C;
end Helper;
"""
text_to_file(helper_ads_text, 'helper.ads')
text_to_file(helper_adb_text, 'helper.adb')

# Emit DC_Unit itself, with 1000 decisions (3 BDD nodes each)
dc_unit_ads_text = """
package DC_Unit is
   procedure Run;
end DC_Unit;
"""
dc_unit_adb_text = [
    'with Helper; use Helper;',
    'package body DC_Unit is',
    '   procedure Run is',
    '   begin'
]
for _ in range(1000):
    dc_unit_adb_text.extend([
        '      if (A and then B) or C then',
        '         raise Program_Error;',
        '      end if;'
    ])
dc_unit_adb_text.extend([
    '   end Run;',
    'end DC_Unit;'
])
text_to_file(dc_unit_ads_text, 'dc_unit.ads')
list_to_file(dc_unit_adb_text, 'dc_unit.adb')

# Start the preparation of source excerpts for the Main unit
main_adb_context_clauses = ['with DC_Unit;']
main_adb_statements = ['DC_Unit.Run;']

# Generate sources for the Simple_Unit_* units
simple_unit_ads = """
package Simple_Unit_{n} is
   procedure Run;
end Simple_Unit_{n};
"""
simple_unit_adb = """
package body Simple_Unit_{n} is
   procedure Run is
   begin
      null;
   end Run;
end Simple_Unit_{n};
"""
for n in range(1, 11):
    text_to_file(simple_unit_ads.format(n=n),
                 'simple_unit_{}.ads'.format(n))
    text_to_file(simple_unit_adb.format(n=n),
                 'simple_unit_{}.adb'.format(n))
    main_adb_context_clauses.append('with Simple_Unit_{};'.format(n))
    main_adb_statements.append('Simple_Unit_{}.Run;'.format(n))

# Finally, generate the Main unit
main_adb_sources = (
    main_adb_context_clauses + ['procedure Main is', 'begin']
    + main_adb_statements + ['end Main;']
)
list_to_file(main_adb_sources, 'main.adb')

# Generate the project file, run the instrumenter, run the program and produce
# a checkpoint.
p = gprfor(mains=['main.adb'], srcdirs=['.'])
build_run_and_coverage(
    gprsw=GPRswitches(root_project=p),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['--save-checkpoint=c0.ckpt'],
    extra_gprbuild_args=['-j128'])

# Load the same checkpoint multiple times. This used to create redundant BDDs,
# making the checkpoint grow over time. Checking that each loading/saving cycle
# does not make the checkpoint grow verifies that this bug is gone.
expected_size = None
for n in range(5):
    prev_checkpoint = 'c{}.ckpt'.format(n)
    next_checkpoint = 'c{}.ckpt'.format(n + 1)
    ckpt_list = 'ckpt_list_{}.txt'.format(n)
    list_to_file([prev_checkpoint] * 50, ckpt_list)
    xcov(['coverage', '-cstmt', '-C@{}'.format(ckpt_list),
          '--save-checkpoint', next_checkpoint])

    size = os.path.getsize(next_checkpoint)
    if expected_size is None:
        expected_size = size
    else:
        thistest.fail_if_not_equal(
            f"Unexpected size for {next_checkpoint}",
            expected_size,
            size,
        )

thistest.result()
