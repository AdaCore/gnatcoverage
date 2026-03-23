with Support, Orelse; use Support;

procedure Test_OrElse_0 is
begin
   Assert (True);
end;

--# orelse.adb
--  /__l!dT-/  l- ## s-
--  /decl/    ~l- ## ~s-
--  /orelse/   l- ## s-
--  /retTrue/  l- ## s-
--  /retFalse/ l- ## s-
--  /retVal/   l- ## s-

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l- ## 0c
