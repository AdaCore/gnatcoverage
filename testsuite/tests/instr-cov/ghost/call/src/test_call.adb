with Support; use Support;

with Common; use Common;
with Ops; use Ops;

procedure Test_Call is
   X : Integer := 15;
begin
   Check_Pos (5);
   Check_Bumpable (X);
   Assert (N_Checks = 1);
end;

--# common.adb
--  /check/ l! ## dT-
--  /raise/ l- ## s-

--# ops.ads

--# ops.adb
--  /nchecks/ l+ ## 0

--%opts:--instrument-ghost
--  =/ghost-if/   l! ## dF-
--  =/ghost-then/ l+ ## 0
--  =/ghost-else/ l- ## s-
--  =/ghost-call/ l+ ## 0
