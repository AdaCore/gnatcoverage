with Assert;
with Ops; use Ops;

procedure Test_Second is
begin
   Assert (Eval (1) = True);
   Assert (Eval (3) = False);
end Test_Second;

--# ops.adb

--%cov: --level=stmt
--  =/cond/     l+ ## 0
--  =/op/       l+ ## 0
--  =/first/    l+ ## 0
--  =/second/   l+ ## 0
--  =/third/    l+ ## 0
--  =/true/     l+ ## 0
--  =/false/    l+ ## 0

--%cov: --level=stmt\+decision
--  =/cond/     l+ ## 0
--  =/op/       l+ ## 0
--  =/first/    l+ ## 0
--  =/second/   l+ ## 0
--  =/third/    l+ ## 0
--  =/true/     l+ ## 0
--  =/false/    l+ ## 0

--%cov: --level=stmt\+(uc_)?mcdc
--  =/cond/     l+ ## 0
--  =/op/       l! ## 0
--  =/first/    l! ## c!
--  =/second/   l! ## 0
--  =/third/    l! ## c!
--  =/true/     l+ ## 0
--  =/false/    l+ ## 0
