--  Test driver for library-level generic instantiations. It calls only a part
--  of the code from instantiations, so only the corresponding part of the code
--  from templates shall be reported as covered.

with New_Value;
with Pack_Instance_Lib_Level;
with Stacks;
with Update;
with Support;         use Support;
procedure Test_Instantiations_Part_2 is
   S : Stacks.Stack;
   I : Integer := 1;
   J : Integer := 2;

   F1 : Float := 1.0;
   F2 : Float := 2.0;
begin
   Assert (Pack_Instance_Lib_Level.Var = 0.0);

   Stacks.Push (I, S);
   Stacks.Pop  (J, S);
   Assert (J = 1);

   Pack_Instance_Lib_Level.Swap (F1, F2);
   Assert (F1 = 2.0 and then F2 = 1.0);
end Test_Instantiations_Part_2;

--# pack.adb
-- /new_value_g/   l- ## s-
-- /swap/          l+ ## 0
-- /proc/          l- ## s-
-- /decl/         ~l- ## ~s-

--#  stacks_g.ads
-- /elab/          l+ ## 0

--#  stacks_g.adb
-- /push/          l+ ## 0
-- /pop/           l+ ## 0
-- /n_values/      l- ## s-
-- /default_stack/ l- ## s-
-- /elab/          l+ ## 0

--# update_g.adb
-- /stmt/          l- ## s-
