--  Test driver for library-level generic instantiations. It only "with's" the
--  code with generic units, but does not with's any instantiation, and does
--  not instantiate anything itself. Code from generic units is expected
--  to be reported as no-code.

with Pack;
with Stacks_G, Update_G;
with Support;         use Support;
procedure Test_Instantiations_No is
begin
   Assert (True);
end Test_Instantiations_No;

--# pack.adb
-- /decl/         ~l- ## ~s-
-- /swap/          l- ## s-
-- /proc/          l- ## s-

--#  stacks_g.ads
-- /decl/          l. ## 0
-- /elab/          l. ## 0
-- /line-elab/     l. ## 0c

--#  stacks_g.adb
-- /push/          l. ## 0
-- /pop/           l. ## 0
-- /n_values/      l. ## 0
-- /default_stack/ l. ## 0
-- /elab/          l. ## 0

--# update_g.adb
-- /stmt/          l. ## 0
