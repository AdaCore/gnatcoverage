--  Test driver for library-level generic instantiations. It "with's" all the
--  functional code, and it calls only the local instantiation of the function
--  Pack.New_Value_G. The code from all the other generics shall be reported as
--  uncovered.

with Pack;
with Local_Instantiations;
with Support;         use Support;
procedure Test_Instantiations_Part_3 is
begin
   Assert (Local_Instantiations.New_Value (0) = 1);
end Test_Instantiations_Part_3;

--# pack.adb
-- /new_value_g/   l+ 0
-- /swap/          l- s-
-- /proc/          l- s-

--#  stacks_g.ads
-- /elab/          l+ 0

--#  stacks_g.adb
-- /push/          l+ 0
-- /pop/           l- s-
-- /n_values/      l- s-
-- /default_stack/ l- s-
-- /elab/          l+ 0

--# update_g.adb
-- /stmt/          l- s-
