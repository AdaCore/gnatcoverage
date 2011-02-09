--  Test driver for exception propagation. It calls the functional subprogram
--  in such a way that Exception_2 is raised in the innermost block. No other
--  exception is raised during the propagation and handling of this exception.
--  All the statements that are skipped as the result of exception propagation
--  are expected to be reported as uncovered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Propagation_Blocks_2 is
  I, J, K : My_Int;
begin
   I := 2;
   J := 100;
   K := 0;

   Proc_With_Blocks (I, J, K);

   Assert (I = 50 and then J = 50 and then  K = 0);
end Test_Propagation_Blocks_2;

--#  pack.adb
-- /if1/            l+ 0
-- /raise1/         l- s-
-- /if2/            l+ 0
-- /raise2/         l+ 0
-- /after_raise2/   l- s-
-- /after_ce_raise/ l- s-
-- /handler_CE/     l- s-
-- /after_block_1/  l- s-
-- /handler_E1/     l- s-
-- /after_block_2/  l- s-
-- /handler_E2/     l+ 0
-- /after_block_3/  l+ 0
-- /handler_others/ l- s-
