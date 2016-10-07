with Support, Expr; use Support, Expr;

procedure Test_2 is
begin
   Assert (Plus (3, 9, Delegate => False) = 12);
end;


--# expr.adb
--  /decl/ l+ ## 0
--  /stmt/ l+ ## 0
--  /test/ l+ ## 0
--  /delegate/ l- ## s-
--  /compute/ l+ ## 0

