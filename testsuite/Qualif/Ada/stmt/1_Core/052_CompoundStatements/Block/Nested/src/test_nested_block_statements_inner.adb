--  Test driver for block statements. It exdecutes the functional code in such
--  a way that all the block statements are executed, so nothing is expected to
--  be reported as uncovered.

with Nested_Block_Statements;
with Support; use Support;
procedure Test_Nested_Block_Statements_Inner is
   I, J : Integer;
begin
   I := 3;
   J := 1;
   Nested_Block_Statements (I, J);
   Assert (I = 7 and then J = 1);
end Test_Nested_Block_Statements_Inner;

--# nested_block_statements.adb

-- /outer_block/  l+ 0
-- /middle_block/ l+ 0
-- /inner_block/  l+ 0
