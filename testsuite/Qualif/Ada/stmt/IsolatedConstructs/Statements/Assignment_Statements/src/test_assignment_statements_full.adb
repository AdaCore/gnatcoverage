--  Test driver for assignment statements. It executes all the functional code
--  so all the assignment statements are expected to be reported as covered.

with Assignment_Statements_Elab;
with Assignment_Statements; use Assignment_Statements;
with Support;               use Support;
procedure Test_Assignment_Statements_Full is
   Result : Integer;
   I, J   : Integer;
begin
   I := 2;
   J := 1;
   Swap (I, J);

   Result := Max_Value (I, J);
   Assert (Result = 2);
end Test_Assignment_Statements_Full;

--# assignment_statements.adb
-- /swap/ l+ 0
-- /max/ l+ 0
-- /ifmax/ l+ 0

--# assignment_statements_elab.adb
-- /elab/ l+ 0
