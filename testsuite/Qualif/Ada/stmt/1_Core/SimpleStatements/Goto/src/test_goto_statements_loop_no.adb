--  Test driver for GOTO statements. It only "with"s the functional code,
--  but does not execute anything from it, so no GOTO statement is expected to
--  be reported as covered.

with GOTO_Statements_Loop; use GOTO_Statements_Loop;
with Support;              use Support;
procedure Test_GOTO_Statements_Loop_No is
begin
   Assert (True);
end Test_GOTO_Statements_Loop_No;

--# goto_statements_loop.adb
-- /1loop/          l- s-
-- /11if/           l- s-
-- /11goto/         l- s-
-- /12if/           l- s-
-- /12goto/         l- s-
-- /1after2goto/    l- s-
-- /13goto/         l- s-
-- /1after3goto/    l- s-
-- /14goto/         l- s-
-- /1after4goto/    l- s-
-- /15goto/         l- s-
-- /1after5goto/    l- s-
-- /1fin/           l- s-

-- /2beforeloop/    l- s-
-- /2loop/          l- s-
-- /21if/           l- s-
-- /21goto/         l- s-
-- /22if/           l- s-
-- /22goto/         l- s-
-- /2after2goto/    l- s-
-- /23goto/         l- s-
-- /2after3goto/    l- s-
-- /24goto/         l- s-
-- /2after4goto/    l- s-
-- /25goto/         l- s-
-- /2after5goto/    l- s-
-- /2fin/           l- s-

-- /3beforeloop/    l- s-
-- /3loop/          ~l- ~s-
-- /3exit/          l- s-
-- /31if/           l- s-
-- /31goto/         l- s-
-- /32if/           l- s-
-- /32goto/         l- s-
-- /3after2goto/    l- s-
-- /33goto/         l- s-
-- /3after3goto/    l- s-
-- /34goto/         l- s-
-- /3after4goto/    l- s-
-- /35goto/         l- s-
-- /3after5goto/    l- s-
-- /3fin/           l- s-
