--  Test driver for local generic instantiations. It "with's" all the
--  functional code, and it calls only routine Swap from the local
--  instantiation of the package Pack.Pack_G. The code from all the other
--  generics shall be reported as uncovered.

with Pack;
with Local_Instantiations;
with Support;         use Support;
procedure Test_Instantiations_Part_4 is
   use Local_Instantiations.Pack_Instance;
   Char : Character := 'B';
begin
   Assert (Var = 'A');
   Swap (Var, Char);
   Assert (Var = 'B');
   Assert (Char = 'A');
end Test_Instantiations_Part_4;

--# pack.adb
-- /new_value_g/   l- ## s-
-- /swap/          l+ ## 0
-- /proc/          l- ## s-

--#  stacks_g.ads
-- /elab/          l+ ## 0

--#  stacks_g.adb
-- /push/          l+ ## 0
-- /pop/           l- ## s-
-- /n_values/      l- ## s-
-- /default_stack/ l- ## s-
-- /elab/          l+ ## 0

--# update_g.adb
-- /stmt/          l- ## s-
