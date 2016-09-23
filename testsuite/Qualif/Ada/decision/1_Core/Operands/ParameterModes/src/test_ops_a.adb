with Support, Ops; use Support, Ops;

procedure Test_Ops_A is
begin
   Assert (Qualify (Step, Pit) = Unsafe);
   Assert (Qualify (Hold, Pit) = Safe);
end;

--# ops.adb
--  /test/    l+ ## 0
--  /unsafe/  l+ ## 0
--  /safe/    l+ ## 0
