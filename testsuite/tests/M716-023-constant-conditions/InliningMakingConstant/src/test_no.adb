with Assert;
with Ops; use Ops;

procedure Test_No is
begin
   null;
end Test_No;

--# ops.adb

--%cov: --level=stmt
--  =/stmt-1/     l- ## s-
--  =/op-1/       l- ## 0
--  =/first-1/    l- ## 0
--  =/second-1/   l- ## 0
--  =/stmt-2/     l- ## s-
--  =/op-2/       l- ## 0
--  =/first-2/    l- ## 0
--  =/second-2/   l- ## 0
--  =/third-2/    l- ## 0

--%cov: --level=stmt\+decision
--  =/stmt-1/     l- ## s-
--  =/op-1/       l- ## 0
--  =/first-1/    l- ## 0
--  =/second-1/   l- ## 0
--  =/stmt-2/     l- ## s-
--  =/op-2/       l- ## 0
--  =/first-2/    l- ## 0
--  =/second-2/   l- ## 0
--  =/third-2/    l- ## 0

--%cov: --level=stmt\+(uc_)?mcdc
--  =/stmt-1/     l- ## s-
--  =/op-1/       l- ## 0
--  =/first-1/    l- ## 0
--  =/second-1/   l- ## 0
--  =/stmt-2/     l- ## s-
--  =/op-2/       l- ## 0
--  =/first-2/    l- ## 0
--  =/second-2/   l- ## 0
--  =/third-2/    l- ## 0
