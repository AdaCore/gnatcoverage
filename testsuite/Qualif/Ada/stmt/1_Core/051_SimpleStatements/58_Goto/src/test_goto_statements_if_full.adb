--  Test driver for GOTO statements. It causes execution of all the functional
--  code, so everything is expected to be reported as covered.

with GOTO_Statements_If; use GOTO_Statements_If;
with Support;            use Support;
procedure Test_GOTO_Statements_If_Full is
   Arg  : Integer;
   Par1 : Integer;
   Par2 : Integer;
begin
   Arg  := 10;
   Par1 := 10;
   Par2 := 10;

   Update (Arg, Par1, Par2,  60, 100);
   Assert (Par2 = 100);

   Arg  := 10;
   Par1 := 10;
   Par2 := 10;

   Update (Arg, Par1, Par2,  5, 5);
   Assert (Arg = 10  and then
           Par1 = 10 and then
           Par2 = 10);

   Arg  := 10;
   Par1 := 9;
   Par2 := 9;

   Update (Arg, Par1, Par2,  5, 5);
   Assert (Arg =  10 and then
           Par1 = 9 and then
           Par2 = 9);

   Arg  := 10;
   Par1 := 9;
   Par2 := 90;

   Update (Arg, Par1, Par2,  5, 5);
   Assert (Arg =  10 and then
           Par1 = 9 and then
           Par2 = 90);

   --  Cases when no GOTO in IF is executed:

   Arg  := 1;
   Par1 := 1;
   Par2 := 9;
   Update (Arg, Par1, Par2,  1, 1);
   Assert (Par1 = 0);

   Arg  := 10;
   Par1 := 1;
   Par2 := 9;
   Update (Arg, Par1, Par2,  1, 1);
   Assert (Par2 = 0);

   Arg  := 10;
   Par1 := 1;
   Par2 := 90;
   Update (Arg, Par1, Par2,  1, 1);
   Assert (Par1 = 1);

end Test_GOTO_Statements_If_Full;

--# goto_statements_if.adb
-- /if/         l+ 0
-- /1if/        l+ 0
-- /1goto/      l+ 0
-- /2if/        l+ 0
-- /2goto/      l+ 0
-- /inif/       l+ 0
-- /elsif/      l+ 0
-- /3if/        l+ 0
-- /3goto/      l+ 0
-- /inelsif/    l+ 0
-- /4if/        l+ 0
-- /4goto/      l+ 0
-- /inelse/     l+ 0
-- /5goto/      l+ 0
-- /after5goto/ l+ 0
-- /fin/        l+ 0
