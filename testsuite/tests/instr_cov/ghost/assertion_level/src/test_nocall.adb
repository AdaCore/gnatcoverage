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
--  /ghost-assert/ l. ## 0
--  /ghost-code/   l- ## s-

--# ops.adb
--  /bump/       l- ## s-
