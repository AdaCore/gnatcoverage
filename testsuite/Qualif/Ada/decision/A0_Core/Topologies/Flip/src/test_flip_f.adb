with Support, Flip; use Support, Flip;

procedure Test_Flip_F is
begin
   Assert (F (False) = True);
end;

--# flip.adb
-- /eval/    l! ## oF-
-- /returnTrue/  l+ ## 0
-- /returnFalse/ l- ## s-
-- /returnVal/   l+ ## 0

-- %opts: --trace-mode=src
-- /ifx-eval/    l! ## dF-

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dT-

-- %opts: --trace-mode=bin
-- /ifx-eval/    l! ## d!
