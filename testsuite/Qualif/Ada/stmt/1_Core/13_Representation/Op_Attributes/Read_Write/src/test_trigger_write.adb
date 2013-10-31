with Support, Trigger; use Support, Trigger;

procedure Test_Trigger_Write is
begin
   Run (Op => Write);
end;

--# trigger.adb
-- /trigger_test/  l+ ## 0
-- /trigger_read/  l- ## s-
-- /trigger_write/ l+ ## 0
