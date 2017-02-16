--  Test driver for library-level generic instantiations. It only "with's" the
--  code with generic units, but does not with's any instantiation, and does
--  not instantiate anything itself. Code from generic units is expected
--  to be reported as no-code, which requires a compiler version > 17.

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

-- %tags:(7.0.2|7.2.2|7.4.3)
-- =/decl/         l- ## s-
-- =/elab/         l- ## s-
-- =/line-elab/    l- ## 0c

--#  stacks_g.adb
-- /push/          l. ## 0
-- /pop/           l. ## 0
-- /n_values/      l. ## 0
-- /default_stack/ l. ## 0
-- /elab/          l. ## 0

-- %tags:(7.0.2|7.2.2|7.4.3)

-- =/push/          l- ## s-
-- =/pop/           l- ## s-
-- =/n_values/      l- ## s-
-- =/default_stack/ l- ## s-
-- =/elab/          l- ## s-

--# update_g.adb
-- /stmt/          l. ## 0

-- %tags:(7.0.2|7.2.2|7.4.3)
-- =/stmt/         l- ## s-
