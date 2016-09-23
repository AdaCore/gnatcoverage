with Support, Ops; use Support, Ops;

procedure Test_Ops_S is
begin
   Assert (Qualify (Step, Ground) = Safe);
   Assert (Qualify (Hold, Pit) = Safe);
end;

--# ops.adb
--  /test/    l! ## dT-
--  /unsafe/  l- ## s-
--  /safe/    l+ ## 0
