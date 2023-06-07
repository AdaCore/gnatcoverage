--  Test driver for checking the coverage information in case of inheritance.
--  The call to Handle in the test driver can be resolved statically
with New_Alert_System;           use New_Alert_System;
with New_Alert_System.Emergency; use New_Alert_System.Emergency;

with Support;                    use Support;

procedure Test_4 is
   EA : Emergency_Alert;
begin
   Handle (EA);
   Assert (not Catastrophe (EA));
end Test_4;

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

--# new_alert_system-emergency.adb
--  /ea_handle/               l+ ## 0
--  /ea_hand_catastrophe/     l- ## s-
--  /ea_hand_no_catastrophe/  l+ ## 0
--  /ea_display/              l- ## s-
--  /ea_displ_catastrophe/    l- ## s-
--  /ea_displ_no_catastrophe/ l- ## s-
--  /ea_log/                  l- ## s-
--  /ea_catastrophe/          l+ ## 0
--  /ea_get/                  l- ## s-
