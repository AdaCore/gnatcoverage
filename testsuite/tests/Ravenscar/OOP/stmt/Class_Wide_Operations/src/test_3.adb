--  Test driver for checking the coverage information in case when the call to
--  a class-wide operation results in dynamic dispatching. Only one
--  implementation of the called discpatching operation is actually called, all
--  the other operations should be reported as fully uncovered

with New_Alert_System;           use New_Alert_System;
with New_Alert_System.Emergency; use New_Alert_System.Emergency;
with New_Alert_System.Objects;   use New_Alert_System.Objects;
with Class_Wide_Ops;             use Class_Wide_Ops;

with Support;                    use Support;

procedure Test_3 is
begin
   Handle_Alert (Empty_High_Alert);
   Assert (Empty_High_Alert.Idx = 0);
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
