--  Test driver for checking the coverage information in case of class-wide
--  operation. Check that when a class-wide operation is not called, nothing
--  from the operation itself as well as from the dispatching operation it can
--  call is reported as covered.

with New_Alert_System;           use New_Alert_System;
with New_Alert_System.Emergency; use New_Alert_System.Emergency;
with New_Alert_System.Objects;   use New_Alert_System.Objects;
with Class_Wide_Ops;             use Class_Wide_Ops;

with Support;                    use Support;

procedure Test_No is
begin
   Assert (True);
end Test_No;

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
--  /ha_log/       l- ## s-

--# new_alert_system-emergency.adb
--  /ea_handle/               l- ## s-
--  /ea_hand_catastrophe/     l- ## s-
--  /ea_hand_no_catastrophe/  l- ## s-
--  /ea_display/              l- ## s-
--  /ea_displ_catastrophe/    l- ## s-
--  /ea_displ_no_catastrophe/ l- ## s-
--  /ea_log/                  l- ## s-
--  /ea_catastrophe/          l- ## s-
--  /ea_get/                  l- ## s-
