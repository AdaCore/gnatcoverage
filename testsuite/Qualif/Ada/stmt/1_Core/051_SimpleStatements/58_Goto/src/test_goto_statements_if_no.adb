--  Test driver for GOTO statements. It only "with"s the functional code,
--  but does not execute anything from it, so no GOTO statement is expected to
--  be reported as covered.

with GOTO_Statements_If; use GOTO_Statements_If;
with Support;            use Support;
procedure Test_GOTO_Statements_If_No is
begin
   Assert (True);
end Test_GOTO_Statements_If_No;

--# goto_statements_if.adb
-- /if/         l- s-
-- /1if/        l- s-
-- /1goto/      l- s-
-- /2if/        l- s-
-- /2goto/      l- s-
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
-- /fin/        l- s-
