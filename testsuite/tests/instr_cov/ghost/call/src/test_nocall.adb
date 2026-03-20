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

--%opts:--instrument-ghost
--  =/ghost-if/   l- ## s-
--  =/ghost-then/ l- ## s-
--  =/ghost-else/ l- ## s-
--  =/ghost-call/ l- ## s-
