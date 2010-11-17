--  Test driver for GOTO statements. It causes execution of the second GOTO
--  statement in IF path in the functional code.

with GOTO_Statements;      use GOTO_Statements;
with Support;              use Support;
procedure Test_GOTO_Statements_If2 is
   Arg  : Integer;
   Par1 : Integer;
   Par2 : Integer;
begin
   Arg  := 10;
   Par1 := 10;
   Par2 := 10;

   Update (Arg, Par1, Par2,  5, 5);
   Assert (Arg = 10  and then
           Par1 = 10 and then
           Par2 = 10);
end Test_GOTO_Statements_If2;

--# goto_statements.adb
-- /if/         l+ 0
-- /1if/        l+ 0
-- /1goto/      l- s-
-- /2if/        l+ 0
-- /2goto/      l+ 0
-- /inif/       l- s-
-- /elsif/      l- s-
-- /3if/        l- s-
-- /3goto/      l- s-
-- /inelsif/    l- s-
-- /4if/        l- s-
-- /4goto/      l- s-
-- /inelse/     l- s-
-- /5goto/      l- s-
-- /after5goto/ l- s-
-- /fin/        l+ 0
