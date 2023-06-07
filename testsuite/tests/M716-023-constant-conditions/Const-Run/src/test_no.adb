with Assert;
with Ops; use Ops;

procedure Test_No is
begin
   null;
end Test_No;

--# ops.adb

--%cov: --level=stmt
--  =/stmt/     l- ## s-
--  =/op/       l- ## 0
--  =/first/    l- ## 0
--  =/second/   l- ## 0

--%cov: --level=stmt\+decision
--  =/stmt/     l- ## s-
--  =/op/       l- ## 0
--  =/first/    l- ## 0
--  =/second/   l- ## 0

--%cov: --level=stmt\+(uc_)?mcdc
--  =/stmt/     l- ## s-
--  =/op/       l- ## 0
--  =/first/    l- ## 0
--  =/second/   l- ## 0
