pragma Ada_2012;

with Support, Expr; use Support, Expr;

procedure Test_Neval is
   Pos : Boolean := False;
begin
   Assert (Plus (5, 6, Pos) = 11);
   Assert (Pos = False);
end;

--# expr.adb
--  /stmt/ l+ ## 0
--  /test/ l+ ## 0
--  /eval/ l- ## s-
