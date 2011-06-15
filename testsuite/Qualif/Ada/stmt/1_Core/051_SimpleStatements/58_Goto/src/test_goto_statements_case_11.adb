--  Test driver for GOTO statements. It causes execution of the first GOTO
--  statement in the first CASE path in the functional code.

with GOTO_Statements_Case; use GOTO_Statements_Case;
with Support;              use Support;
procedure Test_GOTO_Statements_Case_11 is
   Arg  : Integer;
   Par1 : Integer;
   Par2 : Integer;
begin
   Arg  := 5;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 7, 13);
   Assert (Par2 = 13);
end Test_GOTO_Statements_Case_11;

--# goto_statements_case.adb
-- /case/         l+ 0
-- /1if/          l+ 0
-- /1goto/        l+ 0
-- /2if/          l- s-
-- /2goto/        l- s-
-- /in1altcase/   l- s-
-- /3if/          l- s-
-- /3goto/        l- s-
-- /in2altcase/   l- s-
-- /4if/          l- s-
-- /4goto/        l- s-
-- /inotherscase/ l- s-
-- /5goto/        l- s-
-- /after5goto/   l+ 0
-- /fin/          l+ 0
