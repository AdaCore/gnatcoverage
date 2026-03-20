with Assert;
with Ops; use Ops;

procedure Test_Outcome_False is
begin
   Assert (Eval (4) = False);
end Test_Outcome_False;

--# ops.adb

--%cov: --level=stmt
--  =/cond/     l+ ## 0
--  =/op/       l+ ## 0
--  =/first/    l+ ## 0
--  =/second/   l+ ## 0
--  =/third/    l+ ## 0
--  =/true/     l- ## s-
--  =/false/    l+ ## 0

--%cov: --level=stmt\+decision
--  =/cond/     l+ ## 0
--  =/op/       l! ## 0
--  =/first/    l! ## dT-
--  =/second/   l! ## 0
--  =/third/    l! ## 0
--  =/true/     l- ## s-
--  =/false/    l+ ## 0

--%cov: --level=stmt\+(uc_)?mcdc
--  =/cond/     l+ ## 0
--  =/op/       l! ## 0
--  =/first/    l! ## dT-
--  =/second/   l! ## 0
--  =/third/    l! ## 0
--  =/true/     l- ## s-
--  =/false/    l+ ## 0
