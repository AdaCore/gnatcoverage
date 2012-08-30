--  Test driver for local generic instantiations. It "with's" all the
--  functional code, and it calls only (some of) the routines from the local
--  instantiation of the package Stack_G. The code from all the other generics
--  shall be reported as uncovered.

with Pack;
with Local_Instantiations;
with Support;         use Support;
procedure Test_Instantiations_Part_1 is
   use Local_Instantiations.Stacks;
   S   : Stack;
   Int : Integer := 0;
begin
   Push (13, S);
   Pop (Int, S);
   Assert (Int = 13);
end Test_Instantiations_Part_1;

--# pack.adb
-- /new_value_g/   ~l- ~s-
-- /swap/          l- s-
-- /proc/          l- s-

--#  stacks_g.ads
-- /elab/          l+ 0

--#  stacks_g.adb
-- /push/          l+ 0
-- /pop/           l+ 0
-- /n_values/      ~l- ~s-
-- /default_stack/ ~l- ~s-
-- /elab/          l+ 0

--# update_g.adb
-- /stmt/          l- s-
