with Support, Ops; use Support, Ops;

procedure Test_Ops_Pos is
begin
   Latch_X (5);
   Assert (Latches = 1);
end;

--# ops.adb
--  /stmt/ l+ ## 0
--  /eval/ l. ## 0

