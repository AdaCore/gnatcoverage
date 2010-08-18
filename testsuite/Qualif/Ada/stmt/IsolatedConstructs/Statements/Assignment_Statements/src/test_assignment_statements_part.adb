--  Test driver for assignment statements. It executes a part of the functional
--  code, so only a part of assignment statements are expected to be reported
--  as covered.

with Assignment_Statements_Elab;
with Assignment_Statements; use Assignment_Statements;
with Support;               use Support;
procedure Test_Assignment_Statements_Part is
   Result : Integer;
begin
   Result := Max_Value (2, 1);
   Assert (Result = 2);
end Test_Assignment_Statements_Part;

--# assignment_statements.adb
-- /swap/ l- s-
-- /max/ l+ 0
-- /ifmax/ l- s-

--# assignment_statements_elab.adb
-- /elab/ l+ 0
