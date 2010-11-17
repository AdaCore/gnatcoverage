--  Test driver for GOTO statements. It causes execution of the second GOTO
--  statement in loop statements in the functional code.

with GOTO_Statements;      use GOTO_Statements;
with Support;              use Support;
procedure Test_GOTO_Statements_2 is
   Test_Sample : Sample :=
     (1 => Identity (-1),
      2 => Identity (2));

   Par1 : Integer;
   Par2 : Integer;
begin
   Par1 := 1;
   Par2 := 0;
   Update_Sample_For (Test_Sample, Par1, Par2);
   Assert (Test_Sample (1) = -1 and then Test_Sample (2) = 0);

   Par1        := 1;
   Par2        := 0;
   Test_Sample := (1 => Identity (-1), 2 => Identity (2));
   Update_Sample_While (Test_Sample, Par1, Par2);
   Assert (Test_Sample (1) = -1 and then Test_Sample (2) = 0);

   Par1        := 1;
   Par2        := 0;
   Test_Sample := (1 => Identity (-1), 2 => Identity (2));
   Update_Sample (Test_Sample, Par1, Par2);
   Assert (Test_Sample (1) = -1 and then Test_Sample (2) = 0);
end Test_GOTO_Statements_2;

--# goto_statements.adb
-- /1loop/          l+ 0
-- /11if/           l+ 0
-- /11goto/         l- s-
-- /12if/           l+ 0
-- /12goto/         l+ 0
-- /1after2goto/    l- s-
-- /13goto/         l- s-
-- /1after3goto/    l- s-
-- /14goto/         l- s-
-- /1after4goto/    l+ 0
-- /15goto/         l+ 0
-- /1after5goto/    l- s-
-- /1fin/           l+ 0

-- /2beforeloop/    l+ 0
-- /2loop/          l+ 0
-- /21if/           l+ 0
-- /21goto/         l- s-
-- /22if/           l+ 0
-- /22goto/         l+ 0
-- /2after2goto/    l- s-
-- /23goto/         l- s-
-- /2after3goto/    l- s-
-- /24goto/         l- s-
-- /2after4goto/    l+ 0
-- /25goto/         l+ 0
-- /2after5goto/    l- s-
-- /2fin/           l+ 0

-- /3beforeloop/    l+ 0
-- /3loop/          ~l+ ~0
-- /3exit/          l+ 0
-- /31if/           l+ 0
-- /31goto/         l- s-
-- /32if/           l+ 0
-- /32goto/         l+ 0
-- /3after2goto/    l- s-
-- /33goto/         l- s-
-- /3after3goto/    l- s-
-- /34goto/         l- s-
-- /3after4goto/    l+ 0
-- /35goto/         l+ 0
-- /3after5goto/    l- s-
-- /3fin/           l+ 0
