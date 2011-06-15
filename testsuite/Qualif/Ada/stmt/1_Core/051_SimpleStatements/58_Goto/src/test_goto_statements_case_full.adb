--  Test driver for GOTO statements. It causes execution of all the functional
--  code, so everything is expected to be reported as covered.

with GOTO_Statements_Case; use GOTO_Statements_Case;
with Support;              use Support;
procedure Test_GOTO_Statements_Case_Full is
   Arg  : Integer;
   Par1 : Integer;
   Par2 : Integer;
begin
   Arg  := 5;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 7, 13);
   Assert (Par2 = 13);

   Arg  := 5;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 3, 1);
   Assert (Arg  = 4   and then
           Par1 = 100 and then
           Par2 = 200);

   Arg  := 10;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 3, 1);
   Assert (Arg  = 4   and then
           Par1 = 100 and then
           Par2 = 200);

   Arg  := 125;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 10, 15);
   Assert (Arg  = 25   and then
           Par1 = 100 and then
           Par2 = 200);

   Arg  := 125;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 10, 15);
   Assert (Arg  = 25   and then
           Par1 = 100 and then
           Par2 = 200);

   --  Cases when no GOTO in CASE statement is executed:

   Arg  := 5;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 0, 100);
   Assert (Par1 = 0);

   Arg  := 10;
   Par1 := 1;
   Par2 := 2;

   Update (Arg, Par1, Par2, 3, 1);
   Assert (Par2 = 0);

   Arg  := 121;
   Par1 := 100;
   Par2 := 200;

   Update (Arg, Par1, Par2, 10, 15);
   Assert (Par1 = 10);

end Test_GOTO_Statements_Case_Full;

--# goto_statements_case.adb
-- /case/         l+ 0
-- /1if/          l+ 0
-- /1goto/        l+ 0
-- /2if/          l+ 0
-- /2goto/        l+ 0
-- /in1altcase/   l+ 0
-- /3if/          l+ 0
-- /3goto/        l+ 0
-- /in2altcase/   l+ 0
-- /4if/          l+ 0
-- /4goto/        l+ 0
-- /inotherscase/ l+ 0
-- /5goto/        l+ 0
-- /after5goto/   l+ 0
-- /fin/          l+ 0
