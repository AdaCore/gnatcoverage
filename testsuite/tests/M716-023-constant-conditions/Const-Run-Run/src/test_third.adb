with Assert;
with Ops; use Ops;

procedure Test_Third is
begin
   Assert (Eval (1) = True);
   Assert (Eval (3) = False);
end Test_Third;

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
--  =/second/   l! ## c!
--  =/third/    l! ## 0
--  =/true/     l+ ## 0
--  =/false/    l+ ## 0
