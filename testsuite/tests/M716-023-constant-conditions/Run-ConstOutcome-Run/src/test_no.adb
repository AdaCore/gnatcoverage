with Assert;
with Ops; use Ops;

procedure Test_No is
begin
   null;
end Test_No;

--# ops.adb

--%cov: --level=stmt
--  =/cond/     l- ## s-
--  =/op/       l- ## 0
--  =/first/    l- ## 0
--  =/second/   l- ## 0
--  =/third/    l- ## 0
--  =/true/     l- ## s-
--  =/false/    l- ## s-

--%cov: --level=stmt\+decision
--  =/cond/     l- ## s-
--  =/op/       l- ## 0
--  =/first/    l- ## 0
--  =/second/   l- ## 0
--  =/third/    l- ## 0
--  =/true/     l- ## s-
--  =/false/    l- ## s-

--%cov: --level=stmt\+(uc_)?mcdc
--  =/cond/     l- ## s-
--  =/op/       l- ## 0
--  =/first/    l- ## 0
--  =/second/   l- ## 0
--  =/third/    l- ## 0
--  =/true/     l- ## s-
--  =/false/    l- ## s-
