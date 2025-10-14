with Support, Flip; use Support, Flip;

procedure Test_Flip_T is
begin
   Assert (F (True) = False);
end;

--# flip.adb
-- /eval/    l! ## oT-
-- /returnTrue/  l- ## s-
-- /returnFalse/ l+ ## 0
-- /returnVal/   l+ ## 0

-- With binary traces, we can't tell the difference between dT- or dF- on
--  unary expressions in if-expressions. With source instrumentation, we can.

-- %opts: --trace-mode=src
-- /ifx-eval/    l! ## dT-

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## d-

-- %opts: --trace-mode=bin
-- /ifx-eval/    l! ## d!
