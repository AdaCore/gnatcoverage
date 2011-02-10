--  Test driver for block statements. It exdecutes the functional code in such
--  a way that only the outer block statement is executed, so statements from
--  nested block statements are expected to be reported as uncovered.

with Nested_Block_Statements;
with Support; use Support;
procedure Test_Nested_Block_Statements_Outer is
   I, J : Integer;
begin
   I := 0;
   J := -1;
   Nested_Block_Statements (I, J);
   Assert (I = -1 and then J = 1);
end Test_Nested_Block_Statements_Outer;

--# nested_block_statements.adb

-- /outer_block/  l+ 0
-- /middle_block/ l- s-
-- /inner_block/  l- s-
