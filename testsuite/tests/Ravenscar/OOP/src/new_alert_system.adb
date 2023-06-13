package body New_Alert_System is

   Current_Idx : Natural := Natural'First;

   function Get_Idx return Natural is
   begin
      Current_Idx := Current_Idx + 1;                -- # get_idx
      return Current_Idx - 1;                        -- # get_idx
   end Get_Idx;

   Current_Time : My_Time := My_Time'First;

   function My_Clock return My_Time is
   begin
      Current_Time := Current_Time + 1;              -- # my_clock
      return Current_Time;                           -- # my_clock
   end My_Clock;

   procedure Set_Time (New_Time : My_Time) is
   begin
      Current_Time := New_Time;                      -- # set_time
   end Set_Time;

   procedure Handle (A : in out Alert) is
   begin
      A.Idx             := Get_Idx;                  -- # a_handle
      A.Time_Of_Arrival := My_Clock;                 -- # a_handle
   end Handle;

   procedure Display (A     : in  Alert;
                      On    : out Device)
   is
   begin
      On := Device (Log (Alert'Class (A))) + Device (A.Time_Of_Arrival); -- # a_display
--      On := Device (Log (Alert'Class (A))) +
--            Device (A.Time_Of_Arrival);
   end Display;

   function Log (A : in Alert) return Natural is
   begin
      return A.Idx;                                  -- # a_log
   end Log;

   procedure Handle (MA : in out Medium_Alert) is
   begin
      Handle (Alert (MA)); -- handle as plain alert  -- # ma_handle
      MA.Action_Officer := Volunteer;                -- # ma_handle
   end Handle;


   procedure Handle (HA : in out High_Alert) is
   begin
      Handle (Medium_Alert (HA)); -- conversion      -- # ha_handle
      HA.Action_Officer := Soldier;                  -- # ha_handle
      Set_Alarm (HA);                                -- # ha_handle
      Display (HA, System_Device);                   -- # ha_handle
   end Handle;

   procedure Set_Alarm (HA: in out High_Alert) is
   begin
      HA.Ring_Alarm_At  := HA.Time_Of_Arrival + 1;   -- # ha_set_alarm
      HA.Action_Officer := Sergeant;                 -- # ha_set_alarm
   end Set_Alarm;

   function Log (HA : in High_Alert) return Natural is
   begin
      return HA.Idx * 10 + 10;                        -- # ha_log
   end Log;
end New_Alert_System;
