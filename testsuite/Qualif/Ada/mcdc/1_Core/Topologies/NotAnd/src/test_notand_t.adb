with Support, Notand; use Support, Notand;

procedure Test_Notand_T is
begin
   Assert (F (False, True) = True);
end;

--# notand.adb
--  /eval(Stmt|Other)/ l! ## oF-
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l- ## s-
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
