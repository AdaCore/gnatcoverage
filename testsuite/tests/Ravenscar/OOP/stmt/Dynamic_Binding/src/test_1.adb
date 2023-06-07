--  Test driver for checking the coverage information in case of inheritance.
--  The call to Handle in the test driver cannot be resolved statically.
with New_Alert_System;         use New_Alert_System;
with New_Alert_System.Objects; use New_Alert_System.Objects;

with Support;                  use Support;

procedure Test_1 is
   LA : Alert'Class := Empty_Low_Alert;
begin
   Handle (LA);
   Assert (LA.Idx = 0);
end Test_1;

--# new_alert_system.adb
--  /get_idx/      l+ ## 0
--  /my_clock/     l+ ## 0
--  /set_time/     l- ## s-
--  /a_handle/     l+ ## 0
--  /a_display/    l- ## s-
--  /a_log/        l- ## s-
--  /ma_handle/    l- ## s-
--  /ha_handle/    l- ## s-
--  /ha_set_alarm/ l- ## s-
--  /ha_log/       l- ## s-
