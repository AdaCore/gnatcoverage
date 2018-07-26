--  Test driver for block statements. It only "with"s the functional code, but
--  does not execute it, so everything is expected to be reported as uncovered.

with Nested_Block_Statements;
with Support; use Support;
procedure Test_0 is
begin
   Assert (True);
end;
--# nested_block_statements.adb

-- /outer_block/  l- ## s-
-- /middle_block/ l- ## s-
-- /inner_block/  l- ## s-
