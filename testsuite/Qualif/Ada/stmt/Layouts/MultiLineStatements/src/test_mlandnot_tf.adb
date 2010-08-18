with Support, Ml_Stmts; use Support, Ml_Stmts;

procedure Test_MlAndNot_TF is
   E : Boolean;
begin
   Eval_And_Not (True, False, E);
   Assert (E = True);
end;

--# ml_stmts.adb
--  /Xcovmark/ l+ 0
