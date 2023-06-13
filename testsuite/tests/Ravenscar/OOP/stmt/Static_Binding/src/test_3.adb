--  Test driver for checking the coverage information in case of inheritance.
--  The call to Handle in the test driver can be resolved statically
with New_Alert_System; use New_Alert_System;

with Support;          use Support;

procedure Test_3 is
   HA : High_Alert;
begin
   Handle (HA);
   Assert (HA.Action_Officer = Sergeant);
end Test_3;

--# new_alert_system.adb
--  /get_idx/      l+ ## 0
--  /my_clock/     l+ ## 0
--  /set_time/     l- ## s-
--  /a_handle/     l+ ## 0
--  /a_display/    l+ ## 0
--  /a_log/        l- ## s-
--  /ma_handle/    l+ ## 0
--  /ha_handle/    l+ ## 0
--  /ha_set_alarm/ l+ ## 0
--  /ha_log/       l+ ## 0
