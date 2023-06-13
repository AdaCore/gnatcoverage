--  Test driver for checking the coverage information in case of inheritance.
--  Checks that in case when an implicitly declared inherited subprogram is
--  called, the coverage information is correctly generated for the explicit
--  subprogram it is derived from

with New_Alert_System;         use New_Alert_System;
with New_Alert_System.Objects; use New_Alert_System.Objects;

with Support;                  use Support;

procedure Test_Inherited is
   Res : Natural := Log (Empty_Medium_Alert);
begin
   Assert (Res = 0);
   null;
end Test_Inherited;

--# new_alert_system.adb
--  /get_idx/      l- ## s-
--  /my_clock/     l- ## s-
--  /set_time/     l- ## s-
--  /a_handle/     l- ## s-
--  /a_display/    l- ## s-
--  /a_log/        l+ ## 0
--  /ma_handle/    l- ## s-
--  /ha_handle/    l- ## s-
--  /ha_set_alarm/ l- ## s-
--  /ha_log/       l- ## s-
