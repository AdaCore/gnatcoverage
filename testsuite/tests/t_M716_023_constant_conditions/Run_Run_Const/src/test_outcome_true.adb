with Assert;
with Ops; use Ops;

procedure Test_Outcome_True is
begin
   Assert (Eval (1) = True);
end Test_Outcome_True;

--# ops.adb

--%cov: --level=stmt
--  =/cond/     l+ ## 0
--  =/op/       l+ ## 0
--  =/first/    l+ ## 0
--  =/second/   l+ ## 0
--  =/third/    l+ ## 0
--  =/true/     l+ ## 0
--  =/false/    l- ## s-

--%cov: --level=stmt\+decision
--  =/cond/     l+ ## 0
--  =/op/       l! ## 0
--  =/first/    l! ## dF-
--  =/second/   l! ## 0
--  =/third/    l! ## 0
--  =/true/     l+ ## 0
--  =/false/    l- ## s-

--%cov: --level=stmt\+(uc_)?mcdc
--  =/cond/     l+ ## 0
--  =/op/       l! ## 0
--  =/first/    l! ## dF-
--  =/second/   l! ## 0
--  =/third/    l! ## 0
--  =/true/     l+ ## 0
--  =/false/    l- ## s-
