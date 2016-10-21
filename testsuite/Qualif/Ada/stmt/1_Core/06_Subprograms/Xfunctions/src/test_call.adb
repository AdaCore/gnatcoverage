with Support, expr; use Support, Expr;

procedure Test_Call is
begin
   Assert (Plus (5, 9) = 14);
end;

--# expr.ads
--  /stmt/  l+ ## 0
--  /decl/  l+ ## 0
