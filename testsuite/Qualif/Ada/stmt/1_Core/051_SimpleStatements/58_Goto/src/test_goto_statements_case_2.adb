--  Test driver for GOTO statements. It causes execution of the GOTO statement
--  in the second CASE path in the functional code.

with GOTO_Statements_Case; use GOTO_Statements_Case;
with Support;              use Support;
procedure Test_GOTO_Statements_Case_2 is
   Arg  : Integer;
   Par1 : Integer;
   Par2 : Integer;
begin
   Arg  := 10;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 3, 1);
   Assert (Arg  = 4   and then
           Par1 = 100 and then
           Par2 = 200);
end Test_GOTO_Statements_Case_2;

--# goto_statements_case.adb
-- /case/         l+ 0
-- /1if/          l- s-
-- /1goto/        l- s-
-- /2if/          l- s-
-- /2goto/        l- s-
-- /in1altcase/   l- s-
-- /3if/          l+ 0
-- /3goto/        l+ 0
-- /in2altcase/   l- s-
-- /4if/          l- s-
-- /4goto/        l- s-
-- /inotherscase/ l- s-
-- /5goto/        l- s-
-- /after5goto/   l- s-
-- /fin/          l+ 0
