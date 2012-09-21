--  Test driver for library-level generic instantiations. It exsecutes all the
--  functional code corresponding to library-level instantiations, so
--  everything is expected to be reported as covered except the code of
--  Pack.Proc.

with New_Value;
with Pack_Instance_Lib_Level;
with Stacks;
with Update;
with Support;         use Support;
procedure Test_Instantiations_Full is
   S : Stacks.Stack := Stacks.Default_Stack;

   I : Integer := 1;
   J : Integer := 2;

   F1 : Float := 1.0;
   F2 : Float := 2.0;
begin
   Assert (Pack_Instance_Lib_Level.Var = 0.0);
   Assert (Stacks.N_Values (S) = 1);

   Stacks.Pop  (J, S);
   Assert (J = 0);

   Pack_Instance_Lib_Level.Swap (F1, F2);
   Assert (F1 = 2.0 and then F2 = 1.0);

   Assert (New_Value (1) = 2);

   Update (J);
   Assert (J = 1);

end Test_Instantiations_Full;

--# pack.adb
-- /new_value_g/   l+ ## 0
-- /swap/          l+ ## 0
-- /proc/          l- ## s-

--#  stacks_g.ads
-- /elab/          l+ ## 0

--#  stacks_g.adb
-- /push/          l+ ## 0
-- /pop/           l+ ## 0
-- /n_values/      l+ ## 0
-- /default_stack/ l+ ## 0
-- /elab/          l+ ## 0

--# update_g.adb
-- /stmt/          l+ ## 0
