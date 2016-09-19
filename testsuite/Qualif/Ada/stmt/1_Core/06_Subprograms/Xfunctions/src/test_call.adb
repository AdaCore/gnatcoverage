with Support, expr; use Support, Expr;

procedure Test_Call is
begin
   Assert (Plus (5, 9) = 14);
end;

--# expr.ads
--  /fnstmt/ l+ ## 0
--  /cont/  l+ ## 0c
--  /decl/  l+ ## 0
