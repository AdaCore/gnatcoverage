with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_0 is
begin
   Assert (True);
end;

--# andthen.adb
--  /__l!dT-/  l- ## s-
--  /decl/    ~l- ## ~s-
--  /andthen/  l- ## s-
--  /retTrue/  l- ## s-
--  /retFalse/ l- ## s-
--  /retVal/   l- ## s-

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l- ## 0c
