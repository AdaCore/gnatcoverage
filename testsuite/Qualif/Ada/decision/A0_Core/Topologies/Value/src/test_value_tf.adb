with Support, Value; use Support, Value;

procedure Test_Value_TF is
begin
   Assert (F (False) = False);
   Assert (F (True) = True);
end;

--# value.adb
-- /eval/    l+ ## 0
-- /returnTrue/  l+ ## 0
-- /returnFalse/ l+ ## 0
-- /returnVal/   l+ ## 0

-- /ifx-eval/    l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
