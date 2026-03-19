with Support, Ops; use Support, Ops;

procedure Test_Ops_W is
begin
   Assert (Qualify (Step, Ground) = Safe);
   Assert (Qualify (Step, Pit) = Unsafe);
end;

--# ops.adb
--  /test/    l! ## c!:"S.A"
--  /unsafe/  l+ ## 0
--  /safe/    l+ ## 0
