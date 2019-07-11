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
--
--%opts: --trace-mode=bin
-- /decl/         ~l- ## ~s-
-- /swap/          l- ## s-
-- /proc/          l- ## s-
--
--%opts: --trace-mode=src
-- /new_value_g/   l- ## s-
-- /decl/          l- ## s-
-- /swap/          l- ## s-
-- /proc/          l- ## s-
--
--
--#  stacks_g.ads
--
--%opts: --trace-mode=bin
-- /decl/          l. ## 0
-- /elab/          l. ## 0
-- /line-elab/     l. ## 0c
--
--%opts: --trace-mode=src
-- /decl/          l- ## s-
-- /elab/          l- ## s-
-- /line-elab/     l- ## 0

-- %tags:(7.0.2|7.2.2|7.4.3)
-- =/decl/         l- ## s-
-- =/elab/         l- ## s-
-- =/line-elab/    l- ## 0c
--
--
--#  stacks_g.adb
--
--%opts: --trace-mode=bin
-- /push/          l. ## 0
-- /pop/           l. ## 0
-- /n_values/      l. ## 0
-- /default_stack/ l. ## 0
-- /elab/          l. ## 0
--
--%opts: --trace-mode=src
-- /push/          l- ## s-
-- /pop/           l- ## s-
-- /n_values/      l- ## s-
-- /default_stack/ l- ## s-
-- /elab/          l- ## s-
--
-- %tags:(7.0.2|7.2.2|7.4.3)
--
-- =/push/          l- ## s-
-- =/pop/           l- ## s-
-- =/n_values/      l- ## s-
-- =/default_stack/ l- ## s-
-- =/elab/          l- ## s-
--
--
--# update_g.adb
--
--%opts: --trace-mode=bin
-- /stmt/          l. ## 0

--%opts: --trace-mode=src
-- /stmt/          l- ## s-

-- %tags:(7.0.2|7.2.2|7.4.3)
-- =/stmt/         l- ## s-
