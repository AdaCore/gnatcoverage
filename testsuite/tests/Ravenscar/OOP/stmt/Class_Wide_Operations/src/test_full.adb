--  Test driver for checking the coverage information in case when the call to
--  a class-wide operation results in dynamic dispatching. All the
--  implementations of the called discpatching operations are called, so
--  coverage information should be reported for all of them

with New_Alert_System;           use New_Alert_System;
with New_Alert_System.Emergency; use New_Alert_System.Emergency;
with New_Alert_System.Objects;   use New_Alert_System.Objects;
with Class_Wide_Ops;             use Class_Wide_Ops;

with Support;                    use Support;

procedure Test_Full is
   EA : Emergency_Alert := Get_Empty_Emergency_Alert;

begin
   Handle_Alert (Empty_Low_Alert);
   Assert (Empty_Low_Alert.Idx = 0);

   Handle_Alert (Empty_Medium_Alert);
   Assert (Empty_Medium_Alert.Idx = 1);

   Handle_Alert (Empty_High_Alert);
   Assert (Empty_High_Alert.Idx = 2);

   Handle_Alert (EA);
   Assert (Log (EA) = 103);
end Test_Full;

--# new_alert_system.adb
--  /get_idx/      l+ ## 0
--  /my_clock/     l+ ## 0
--  /set_time/     l- ## s-
--  /a_handle/     l+ ## 0
--  /a_display/    l+ ## 0
--  /a_log/        l+ ## 0
--  /ma_handle/    l+ ## 0
--  /ha_handle/    l+ ## 0
--  /ha_set_alarm/ l+ ## 0
--  /ha_log/       l+ ## 0

--# new_alert_system-emergency.adb
--  /ea_handle/               l+ ## 0
--  /ea_hand_catastrophe/     l- ## s-
--  /ea_hand_no_catastrophe/  l+ ## 0
--  /ea_display/              l- ## s-
--  /ea_displ_catastrophe/    l- ## s-
--  /ea_displ_no_catastrophe/ l- ## s-
--  /ea_log/                  l+ ## 0
--  /ea_catastrophe/          l- ## s-
--  /ea_get/                  l+ ## 0
