--  Test driver for block statements. It exdecutes the functional code in such
--  a way that all the block statements except the innermost one are executed,
--  so statements from the innermost block statement are expected to be reported
--  as uncovered.

with Nested_Block_Statements;
with Support; use Support;
procedure Test_Nested_Block_Statements_Middle is
   I, J : Integer;
begin
   I := -1;
   J := 1;
   Nested_Block_Statements (I, J);
   Assert (I = 2 and then J = 0);
end Test_Nested_Block_Statements_Middle;

--# nested_block_statements.adb

-- /outer_block/  l+ 0
-- /middle_block/ l+ 0
-- /inner_block/  l- s-
