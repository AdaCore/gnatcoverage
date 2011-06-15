--  Test driver for exception propagation. It only "with's" the functional
--  code, but does not call anything from it, so everything is expected to be
--  reported as uncovered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Propagation_Blocks_No is
begin
   Assert (True);
end Test_Propagation_Blocks_No;

--#  pack.adb
-- /if1/            l- s-
-- /raise1/         l- s-
-- /if2/            l- s-
-- /raise2/         l- s-
-- /after_raise2/   l- s-
-- /after_ce_raise/ l- s-
-- /handler_CE/     l- s-
-- /after_block_1/  l- s-
-- /handler_E1/     l- s-
-- /after_block_2/  l- s-
-- /handler_E2/     l- s-
-- /after_block_3/  l- s-
-- /handler_others/ l- s-
