with Support; use Support;

with Common; use Common;
with Ops; use Ops;

procedure Test_NoCall is
begin
   Check_Pos (5);
end;

--# common.adb
--  /check/ l! ## dT-
--  /raise/ l- ## s-

--# ops.ads

--# ops.adb
--  /nchecks/ l- ## s-
--  /ghost/   l. ## 0
--%opts:--instrument-ghost
--  =/ghost/  l- ## s-
