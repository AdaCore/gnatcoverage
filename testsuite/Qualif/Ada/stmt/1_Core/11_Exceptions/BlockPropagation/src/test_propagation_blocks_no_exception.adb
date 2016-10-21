--  Test driver for exception propagation. It calls the functional subprogram
--  in such a way that no exception is raised. All the code in exception
--  handlers is expected to be reported as uncovered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Propagation_Blocks_No_Exception is
  I, J, K : My_Int;
begin
   I := 2;
   J := 1;
   K := 0;

   Proc_With_Blocks (I, J, K);

   Assert (I = 61 and then J = 60 and then  K = 10);
end Test_Propagation_Blocks_No_Exception;

--#  pack.adb
-- /if1/            l+ ## 0
-- /raise1/         l- ## s-
-- /if2/            l+ ## 0
-- /raise2/         l- ## s-
-- /after_raise2/   l+ ## 0
-- /after_ce_raise/ l+ ## 0
-- /handler_CE/     l- ## s-
-- /after_block_1/  l+ ## 0
-- /handler_E1/     l- ## s-
-- /after_block_2/  l+ ## 0
-- /handler_E2/     l- ## s-
-- /after_block_3/  l+ ## 0
-- /handler_others/ l- ## s-
