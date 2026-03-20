with Support, Trigger; use Support, Trigger;

procedure Test_Trigger_RW is
begin
   Run (Op => Read);
   Run (Op => Write);
end;

--# trigger.adb
-- /trigger_test/  l+ ## 0
-- /trigger_read/  l+ ## 0
-- /trigger_write/ l+ ## 0
