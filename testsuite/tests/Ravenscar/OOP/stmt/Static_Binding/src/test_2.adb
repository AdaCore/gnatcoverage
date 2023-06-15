--  Test driver for checking the coverage information in case of inheritance.
--  The call to Handle in the test driver can be resolved statically
with New_Alert_System; use New_Alert_System;

with Support;          use Support;

procedure Test_2 is
   MA : Medium_Alert;
begin
   Handle (MA);
   Assert (MA.Action_Officer = Volunteer);
end Test_2;

--# new_alert_system.adb
--  /get_idx/      l+ ## 0
--  /my_clock/     l+ ## 0
--  /set_time/     l- ## s-
--  /a_handle/     l+ ## 0
--  /a_display/    l- ## s-
--  /a_log/        l- ## s-
--  /ma_handle/    l+ ## 0
--  /ha_handle/    l- ## s-
--  /ha_set_alarm/ l- ## s-
--  /ha_log/       l- ## s-
