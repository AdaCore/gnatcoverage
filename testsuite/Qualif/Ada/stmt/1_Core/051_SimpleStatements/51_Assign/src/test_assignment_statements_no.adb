--  Test driver for assignment statements. It only "with"s the functional code,
--  but does not execute anything from it, so all the assignment statements are
--  expected to be reported as incovered.

with Assignment_Statements; use Assignment_Statements;
with Support;               use Support;
procedure Test_Assignment_Statements_No is
begin
   Assert (True);
end Test_Assignment_Statements_No;

--# assignment_statements.adb
-- /swap/  l- ## s-
-- /max/   l- ## s-
-- /ifmax/ l- ## s-
