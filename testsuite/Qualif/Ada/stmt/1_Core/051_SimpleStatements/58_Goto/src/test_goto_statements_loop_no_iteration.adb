--  Test driver for GOTO statements. It executes only conditional loops from
--  the functional code, and loop conditions prevent the first loop iterations.
--  So no GOTO statement in side loops are expected to be reported as covered.

with GOTO_Statements_loop; use GOTO_Statements_loop;
with Support;              use Support;
procedure Test_GOTO_Statements_Loop_No_Iteration is
   Null_Test_Sample : Sample (Identity (1) .. Identity (0)) :=
     (others => Identity (2));

   Par1 : Integer;
   Par2 : Integer;
begin
   Par1 := 1;
   Par2 := 1;
   Update_Sample_For (Null_Test_Sample, Par1, Par2);
   Assert (Par1 = 0 and then Par2 = 0);

   Par1 := 1;
   Par2 := 1;
   Update_Sample_While (Null_Test_Sample, Par1, Par2);
   Assert (Par1 = 0 and then Par2 = 0);

   Par1 := 1;
   Par2 := 1;
   Update_Sample (Null_Test_Sample, Par1, Par2);
   Assert (Par1 = 0 and then Par2 = 0);
end Test_GOTO_Statements_loop_No_Iteration;

--# goto_statements_loop.adb
-- /1loop/          l+ ## 0
-- /11if/           l- ## s-
-- /11goto/         l- ## s-
-- /12if/           l- ## s-
-- /12goto/         l- ## s-
-- /1after2goto/    l- ## s-
-- /13goto/         l+ ## 0
-- /1after3goto/    l- ## s-
-- /14goto/         l- ## s-
-- /1after4goto/    l- ## s-
-- /15goto/         l- ## s-
-- /1after5goto/    l+ ## 0
-- /1fin/           l+ ## 0

-- /2beforeloop/    l+ ## 0
-- /2loop/          l+ ## 0
-- /21if/           l- ## s-
-- /21goto/         l- ## s-
-- /22if/           l- ## s-
-- /22goto/         l- ## s-
-- /2after2goto/    l- ## s-
-- /23goto/         l+ ## 0
-- /2after3goto/    l- ## s-
-- /24goto/         l- ## s-
-- /2after4goto/    l- ## s-
-- /25goto/         l- ## s-
-- /2after5goto/    l+ ## 0
-- /2fin/           l+ ## 0

-- /3beforeloop/    l+ ## 0
-- /3loop/          ~l+ ## ~0
-- /3exit/          l+ ## 0
-- /31if/           l- ## s-
-- /31goto/         l- ## s-
-- /32if/           l- ## s-
-- /32goto/         l- ## s-
-- /3after2goto/    l- ## s-
-- /33goto/         l+ ## 0
-- /3after3goto/    l- ## s-
-- /34goto/         l- ## s-
-- /3after4goto/    l- ## s-
-- /35goto/         l- ## s-
-- /3after5goto/    l+ ## 0
-- /3fin/           l+ ## 0
