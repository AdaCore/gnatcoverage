--  Test driver for library-level generic instantiations. It only "with's" the
--  functional code, including instantiations, but does not call anything from
--  it. So the only code that shall be reported as being covered is the
--  elaboration code corresponding to package instantiations.

with New_Value;
with Pack_Instance_Lib_Level;
with Stacks;
with Update;
with Support;         use Support;
procedure Test_Instantiations_Elab_Only is
begin
   Assert (Pack_Instance_Lib_Level.Var = 0.0);
end Test_Instantiations_Elab_Only;

--# pack.adb
-- /new_value_g/   l- ## s-
-- /swap/          l- ## s-
-- /proc/          l- ## s-
-- /decl/         ~l- ## ~s-

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
