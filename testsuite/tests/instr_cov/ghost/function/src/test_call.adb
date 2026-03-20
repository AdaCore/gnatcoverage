with Support; use Support;

with Common; use Common;
with Ops; use Ops;

procedure Test_Call is
   X : Integer := 12;
begin
   Check_Pos (5);
   Bump (X);
   Assert (X = 13);
end;

--# common.adb
--  /check/ l! ## dT-
--  /raise/ l- ## s-

--# ops.ads
--  /ghost-assert/ l. ## 0

--# ops.adb
--  /bump/  l+ ## 0
--%opts:instrument-ghost
--  /ghost-if/   l! ## dF-
--  /ghost-then/ l+ ## 0
--  /ghost-else/ l- ## s-
