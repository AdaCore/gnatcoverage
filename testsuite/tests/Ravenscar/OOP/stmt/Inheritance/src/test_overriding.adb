--  Test driver for checking the coverage information in case of inheritance.
--  Checks that in case when an explicitely declared subprogram that ovverrides
--  the implicitely declared inherited subprogram is called, the coverage
--  information is correctly generated for this explicit subprogram, but not
--  for the (explicitely declared) subprogram from that the overriden
--  subprogram is inherited

with New_Alert_System;         use New_Alert_System;
with New_Alert_System.Objects; use New_Alert_System.Objects;

with Support;                  use Support;

procedure Test_Overriding is
   Res : Natural := Log (Empty_High_Alert);
begin
   Assert (Res = 10);
   null;
end;

--# new_alert_system.adb
--  /get_idx/      l- ## s-
--  /my_clock/     l- ## s-
--  /set_time/     l- ## s-
--  /a_handle/     l- ## s-
--  /a_display/    l- ## s-
--  /a_log/        l- ## s-
--  /ma_handle/    l- ## s-
--  /ha_handle/    l- ## s-
--  /ha_set_alarm/ l- ## s-
--  /ha_log/       l+ ## 0
