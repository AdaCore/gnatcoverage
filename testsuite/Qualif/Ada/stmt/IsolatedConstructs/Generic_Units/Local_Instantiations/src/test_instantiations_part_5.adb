--  Test driver for library-level generic instantiations. It "with's" all the
--  functional code, and it calls only routine
--  Local_Instantiations.Proc_With_Instantiations that instantiates generic
--  packages Pack.Pack_G and Stacks_G and calls some routines from the
--  instantiations. The code from all the other generics shall be reported as
--  uncovered.

with Pack;
with Local_Instantiations;
with Support;         use Support;
procedure Test_Instantiations_Part_5 is
   Int1  : Integer := 1;
   Int2  : Integer := 2;
   Bool1 : Boolean := True;
   Bool2 : Boolean := False;
begin
   Local_Instantiations.Proc_With_Instantiations
     (I  => Int1,
      J  => Int2,
      B1 => Bool1,
      B2 => Bool2);

   Assert (Int1 = 2);
   Assert (Int2 = 1);
   Assert (not Bool1);
   Assert (Bool2);
end Test_Instantiations_Part_5;

--# pack.adb
-- /new_value_g/   l- s-
-- /swap/          l+ 0
-- /proc/          l- s-

--#  stacks_g.ads
-- /elab/          l+ 0

--#  stacks_g.adb
-- /push/          l+ 0
-- /pop/           l+ 0
-- /n_values/      l+ 0
-- /default_stack/ l+ 0
-- /elab/          l+ 0

--# update_g.adb
-- /stmt/          l- s-
