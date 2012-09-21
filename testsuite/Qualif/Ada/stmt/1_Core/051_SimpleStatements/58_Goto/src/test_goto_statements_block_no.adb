--  Test driver for GOTO statements. It only "with"s the functional code,
--  but does not execute anything from it, so no GOTO statement is expected to
--  be reported as covered.

with GOTO_Statements_Block; use GOTO_Statements_Block;
with Support;               use Support;
procedure Test_GOTO_Statements_Block_No is
begin
   Assert (True);
end Test_GOTO_Statements_Block_No;

--# goto_statements_block.adb
-- /1inblock/   l- ## s-
-- /1if/        l- ## s-
-- /in1if/      l- ## s-
-- /1goto/      l- ## s-
-- /2block/     l- ## s-
-- /2if/        ~l- ## ~s-
-- /in2if/      ~l- ## ~s-
-- /2goto/      ~l- ## ~s-
-- /after2goto/ ~l- ## ~s-
-- /afterblock/ l- ## s-
-- /fin/        l- ## s-
