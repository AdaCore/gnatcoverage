with Support, Ml_Stmts; use Support, Ml_Stmts;

--  Don't call anything and verify that a "statement not covered"
--  violation is reported only for lines with a start of statement.

procedure Test_MlAndNot_0 is
begin
   Assert (True);
end;

--# ml_stmts.adb
--  /Linemark/      l- 0
--  /Statementmark/ l- s-
