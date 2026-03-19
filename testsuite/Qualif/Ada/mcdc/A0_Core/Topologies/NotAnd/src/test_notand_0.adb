with Support, Notand; use Support, Notand;

procedure Test_Notand_0 is
begin
   Assert (True);
end;

--# notand.adb
--  /evalStmt/      l- ## s-
--  /evalOther/     l- ## 0c
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l- ## s-
--  /returnValue/   l- ## s-
--  /decl/         ~l- ## ~s-

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l- ## 0c
