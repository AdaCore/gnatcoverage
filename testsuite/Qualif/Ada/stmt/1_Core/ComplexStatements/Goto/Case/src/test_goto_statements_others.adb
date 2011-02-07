--  Test driver for GOTO statements. It causes execution of the GOTO statement
--  in the OTHERS CASE path in the functional code.

with GOTO_Statements;      use GOTO_Statements;
with Support;              use Support;
procedure Test_GOTO_Statements_Others is
   Arg  : Integer;
   Par1 : Integer;
   Par2 : Integer;
begin
   Arg  := 125;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 10, 15);
   Assert (Arg  = 25   and then
           Par1 = 100 and then
           Par2 = 200);
end Test_GOTO_Statements_Others;

--# goto_statements.adb
-- /case/         l+ 0
-- /1if/          l- s-
-- /1goto/        l- s-
-- /2if/          l- s-
-- /2goto/        l- s-
-- /in1altcase/   l- s-
-- /3if/          l- s-
-- /3goto/        l- s-
-- /in2altcase/   l- s-
-- /4if/          l+ 0
-- /4goto/        l+ 0
-- /inotherscase/ l- s-
-- /5goto/        l- s-
-- /after5goto/   l- s-
-- /fin/          l+ 0
