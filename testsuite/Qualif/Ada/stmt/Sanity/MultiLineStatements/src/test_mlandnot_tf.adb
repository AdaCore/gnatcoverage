with Support, Ml_Stmts; use Support, Ml_Stmts;

--  Call and verify that nothing is reported uncovered.

procedure Test_MlAndNot_TF is
   E : Boolean;
begin
   Eval_And_Not (True, False, E);
   Assert (E = True);
end;

--# ml_stmts.adb
--  /Statementmark/ l+ 0
--  /Linemark/      l+ 0
