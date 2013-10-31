with Support, Trigger; use Support, Trigger;

procedure Test_Trigger_Read is
begin
   Run (Op => Read);
end;

--# trigger.adb
-- /trigger_test/  l+ ## 0
-- /trigger_read/  l+ ## 0
-- /trigger_write/ l- ## s-
