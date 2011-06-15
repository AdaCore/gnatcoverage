--  Test driver for library-level generic instantiations. It calls routines
--  from the local instantiations that cause all the code from generics to be
--  reported as covered

with Pack;
with Local_Instantiations;
with Support;         use Support;
procedure Test_Instantiations_Full is
   Int1 : Integer := 1;
   Int2 : Integer := 2;

   use Local_Instantiations.Stacks;
   S : Stack := Default_Stack;
begin
   Pack.Proc (Int1, Int2);
   Assert (Int1 = 2 and then Int2 = 1);

   Assert (Local_Instantiations.New_Value (0) = 1);

   Pop (Int1, S);
   Assert (Int1 = 1);

   Assert (N_Values (S) = 0);

   Local_Instantiations.Update (Int1);
   Assert (Int1 = 2);

end Test_Instantiations_Full;

--# pack.adb
-- /new_value_g/   l+ 0
-- /swap/          l+ 0
-- /proc/          l+ 0

--#  stacks_g.ads
-- /elab/          l+ 0

--#  stacks_g.adb
-- /push/          l+ 0
-- /pop/           l+ 0
-- /n_values/      l+ 0
-- /default_stack/ l+ 0
-- /elab/          l+ 0

--# update_g.adb
-- /stmt/          l+ 0
