--  Test driver for GOTO statements. It causes execution of all the statements
--  in functional code, so everything is expected to be reported as covered.

with GOTO_Statements_Loop; use GOTO_Statements_Loop;
with Support;              use Support;
procedure Test_GOTO_Statements_Loop_Full is

   Test_Sample_1 : Sample :=
     (1 => Identity (1),
      2 => Identity (2));

   Test_Sample_2 : Sample :=
     (1 => Identity (-1),
      2 => Identity (2));

   Test_Sample_3 : Sample (Identity (1) .. Identity (3)) :=
     (others => Identity (0));

   Par1 : Integer := 1;
   Par2 : Integer := 0;
begin
   ----------------------
   -- Testing FOR loop --
   ----------------------

   Par1 := 1;
   Par2 := 1;

   Update_Sample_For (Test_Sample_1, Par1, Par2);
   Assert (Test_Sample_1 (1) = 0 and then Test_Sample_1 (2) = 2);

   Par1 := 1;
   Par2 := 0;
   Update_Sample_For (Test_Sample_2, Par1, Par2);
   Assert (Test_Sample_2 (1) = -1 and then Test_Sample_2 (2) = 0);

   Update_Sample_For (Test_Sample_3, Par1, Par2);
   Assert (Test_Sample_3 (1) = 1 and then
           Test_Sample_3 (2) = 2 and then
           Test_Sample_3 (3) = 3);

   ------------------------
   -- Testing WHILE loop --
   ------------------------

   Test_Sample_1 := (1 => Identity (1), 2 => Identity (2));
   Test_Sample_2 := (1 => Identity (-1), 2 => Identity (2));
   Test_Sample_3 := (others => Identity (0));

   Par1 := 1;
   Par2 := 1;

   Update_Sample_While (Test_Sample_1, Par1, Par2);
   Assert (Test_Sample_1 (1) = 0 and then Test_Sample_1 (2) = 2);

   Par1 := 1;
   Par2 := 0;
   Update_Sample_While (Test_Sample_2, Par1, Par2);
   Assert (Test_Sample_2 (1) = -1 and then Test_Sample_2 (2) = 0);

   Update_Sample_While (Test_Sample_3, Par1, Par2);
   Assert (Test_Sample_3 (1) = 1 and then
           Test_Sample_3 (2) = 2 and then
           Test_Sample_3 (3) = 3);

   --------------------------------
   -- Testing unconditional loop --
   --------------------------------

   Test_Sample_1 := (1 => Identity (1), 2 => Identity (2));
   Test_Sample_2 := (1 => Identity (-1), 2 => Identity (2));
   Test_Sample_3 := (others => Identity (0));

   Par1 := 1;
   Par2 := 1;

   Update_Sample (Test_Sample_1, Par1, Par2);
   Assert (Test_Sample_1 (1) = 0 and then Test_Sample_1 (2) = 2);

   Par1 := 1;
   Par2 := 0;
   Update_Sample (Test_Sample_2, Par1, Par2);
   Assert (Test_Sample_2 (1) = -1 and then Test_Sample_2 (2) = 0);

   Update_Sample (Test_Sample_3, Par1, Par2);
   Assert (Test_Sample_3 (1) = 1 and then
           Test_Sample_3 (2) = 2 and then
           Test_Sample_3 (3) = 3);

end Test_GOTO_Statements_Loop_Full;

--# goto_statements_loop.adb
-- /1loop/          l+ ## 0
-- /11if/           l+ ## 0
-- /11goto/         l+ ## 0
-- /12if/           l+ ## 0
-- /12goto/         l+ ## 0
-- /1after2goto/    l+ ## 0
-- /13goto/         l+ ## 0
-- /1after3goto/    l+ ## 0
-- /14goto/         l+ ## 0
-- /1after4goto/    l+ ## 0
-- /15goto/         l+ ## 0
-- /1after5goto/    l+ ## 0
-- /1fin/           l+ ## 0

-- /2beforeloop/    l+ ## 0
-- /2loop/          l+ ## 0
-- /21if/           l+ ## 0
-- /21goto/         l+ ## 0
-- /22if/           l+ ## 0
-- /22goto/         l+ ## 0
-- /2after2goto/    l+ ## 0
-- /23goto/         l+ ## 0
-- /2after3goto/    l+ ## 0
-- /24goto/         l+ ## 0
-- /2after4goto/    l+ ## 0
-- /25goto/         l+ ## 0
-- /2after5goto/    l+ ## 0
-- /2fin/           l+ ## 0

-- /3beforeloop/    l+ ## 0
-- /3loop/          ~l+ ## ~0
-- /3exit/          l+ ## 0
-- /31if/           l+ ## 0
-- /31goto/         l+ ## 0
-- /32if/           l+ ## 0
-- /32goto/         l+ ## 0
-- /3after2goto/    l+ ## 0
-- /33goto/         l+ ## 0
-- /3after3goto/    l+ ## 0
-- /34goto/         l+ ## 0
-- /3after4goto/    l+ ## 0
-- /35goto/         l+ ## 0
-- /3after5goto/    l+ ## 0
-- /3fin/           l+ ## 0
