with Support, Value; use Support, Value;

procedure Test_Value_F is
begin
   Assert (F (False) = False);
end;

--# value.adb
-- /eval/    l! ## oT-
-- /returnTrue/  l- ## s-
-- /returnFalse/ l+ ## 0
-- /returnVal/   l+ ## 0

-- %opts: --trace-mode=src
-- /ifx-eval/    l! ## dT-

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## d-

-- %opts: --trace-mode=bin
-- /ifx-eval/    l! ## d!
