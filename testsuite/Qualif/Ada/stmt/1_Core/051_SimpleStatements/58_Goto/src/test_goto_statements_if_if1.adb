--  Test driver for GOTO statements. It causes execution of the first GOTO
--  statement in IF path in the functional code.

with GOTO_Statements_If; use GOTO_Statements_If;
with Support;            use Support;
procedure Test_GOTO_Statements_If_If1 is
   Arg  : Integer;
   Par1 : Integer;
   Par2 : Integer;
begin
   Arg  := 10;
   Par1 := 10;
   Par2 := 10;

   Update (Arg, Par1, Par2,  60, 100);
   Assert (Par2 = 100);
end Test_GOTO_Statements_If_If1;

--# goto_statements_if.adb
-- /if/         l+ ## 0
-- /1if/        l+ ## 0
-- /1goto/      l+ ## 0
-- /2if/        l- ## s-
-- /2goto/      l- ## s-
-- /inif/       l- ## s-
-- /elsif/      l- ## s-
-- /3if/        l- ## s-
-- /3goto/      l- ## s-
-- /inelsif/    l- ## s-
-- /4if/        l- ## s-
-- /4goto/      l- ## s-
-- /inelse/     l- ## s-
-- /5goto/      l- ## s-
-- /after5goto/ l+ ## 0
-- /fin/        l+ ## 0
