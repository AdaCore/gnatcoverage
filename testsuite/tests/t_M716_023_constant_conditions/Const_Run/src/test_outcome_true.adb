with Assert;
with Ops; use Ops;

procedure Test_Outcome_True is
begin
   Assert (Eval (1) = True);
end Test_Outcome_True;

--# ops.adb

--%cov: --level=stmt
--  =/stmt/     l+ ## 0
--  =/op/       l+ ## 0
--  =/first/    l+ ## 0
--  =/second/   l+ ## 0

--%cov: --level=stmt\+decision
--  =/stmt/     l+ ## 0
--  =/op/       l+ ## 0
--  =/first/    l+ ## 0
--  =/second/   l+ ## 0

--%cov: --level=stmt\+(uc_)?mcdc
--  =/stmt/     l+ ## 0
--  =/op/       l! ## 0
--  =/first/    l! ## eF-
--  =/second/   l! ## 0
