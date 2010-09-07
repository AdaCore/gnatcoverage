--  Test driver for library-level generic instantiations. It only "with's" the
--  functional code and code that instantiates generics of interest, but does
--  not use anything from it. So the only code that shall be reported as being
--  covered is the elaboration code corresponding to package instantiations.

with Pack;
with Local_Instantiations;
with Support;         use Support;
procedure Test_Instantiations_Elab_Only is
begin
   Assert (Local_Instantiations.Pack_Instance.Var = 'A');
end Test_Instantiations_Elab_Only;

--# pack.adb
-- /new_value_g/   l- s-
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
