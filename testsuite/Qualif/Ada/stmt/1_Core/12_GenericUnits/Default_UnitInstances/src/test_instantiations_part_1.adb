--  Test driver for library-level generic instantiations. It calls only a part
--  of the code from instantiations, so only the corresponding part of the code
--  from templates shall be reported as covered.

with New_Value;
with Pack_Instance_Lib_Level;
with Stacks;
with Update;
with Support;         use Support;
procedure Test_Instantiations_Part_1 is
   S : Stacks.Stack := Stacks.Default_Stack;
begin
   Assert (Pack_Instance_Lib_Level.Var = 0.0);

   Assert (New_Value (0) = 1);

   Assert (Stacks.N_Values (S) = 1);
end Test_Instantiations_Part_1;

--# pack.adb
-- /new_value_g/   l+ ## 0
-- /swap/          l- ## s-
-- /proc/          l- ## s-
-- /decl/         ~l- ## ~s-

--#  stacks_g.ads
-- /elab/          l+ ## 0

--#  stacks_g.adb
-- /push/          l+ ## 0
-- /pop/           l- ## s-
-- /n_values/      l+ ## 0
-- /default_stack/ l+ ## 0
-- /elab/          l+ ## 0

--# update_g.adb
-- /stmt/          l- ## s-
