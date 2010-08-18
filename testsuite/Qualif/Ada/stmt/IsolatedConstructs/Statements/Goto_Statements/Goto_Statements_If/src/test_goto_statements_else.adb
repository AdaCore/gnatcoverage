--  Test driver for GOTO statements. It causes execution of the GOTO statement
--  in ELSE path in the functional code.

with GOTO_Statements;      use GOTO_Statements;
with Support;              use Support;
procedure Test_GOTO_Statements_Else is
   Arg  : Integer;
   Par1 : Integer;
   Par2 : Integer;
begin
   Arg  := 10;
   Par1 := 9;
   Par2 := 90;

   Update (Arg, Par1, Par2,  5, 5);
   Assert (Arg =  10 and then
           Par1 = 9 and then
           Par2 = 90);
end Test_GOTO_Statements_Else;

--# goto_statements.adb
-- /if/         l+ 0
-- /1if/        l- s-
-- /1goto/      l- s-
-- /2if/        l- s-
-- /2goto/      l- s-
-- /inif/       l- s-
-- /elsif/      l+ 0
-- /3if/        l- s-
-- /3goto/      l- s-
-- /inelsif/    l- s-
-- /4if/        l+ 0
-- /4goto/      l+ 0
-- /inelse/     l- s-
-- /5goto/      l- s-
-- /after5goto/ l- s-
-- /fin/        l+ 0
