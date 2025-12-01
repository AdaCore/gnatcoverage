with Support; use Support;

with Common; use Common;
with Ops; use Ops;

procedure Test_Call is
begin
   Check_Pos (5);
   Monitor (A => True, B => True);
   Assert (N_Checks = 1);
end;

--# common.adb
--  /check/ l! ## dT-
--  /raise/ l- ## s-

--# ops.ads

--# ops.adb
--  /nchecks/ l+ ## 0
--  /ghost/   l. ## 0
--%opts:--instrument-ghost
--  =/ghost/   l! ## eF-
