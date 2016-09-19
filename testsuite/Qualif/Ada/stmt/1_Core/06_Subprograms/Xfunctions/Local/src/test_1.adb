with Support, Expr; use Support, Expr;

procedure Test_1 is
begin
   Assert (Plus (3, 9) = 12);
end;


--# expr.adb
--  /decl/ l+ ## 0
--  /stmt/ l+ ## 0
--  /eval/ l- ## s-

