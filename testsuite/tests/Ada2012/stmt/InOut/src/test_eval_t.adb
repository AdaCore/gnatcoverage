pragma Ada_2012;

with Support, Expr; use Support, Expr;

procedure Test_Eval_T is
   Pos : Boolean := True;
begin
   Assert (Plus (5, 6, Pos) = 11);
   Assert (Pos = True);
end;

--# expr.adb
--  /stmt/ l+ ## 0
--  /test/ l+ ## 0
--  /eval/ l+ ## 0

