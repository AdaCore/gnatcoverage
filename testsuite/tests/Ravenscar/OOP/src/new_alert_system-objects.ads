package New_Alert_System.Objects is
   Empty_Alert : Alert :=
     (Idx             => 0,
      Time_Of_Arrival => Zero_Time,
      Message         => No_Message);

   Empty_Low_Alert : Low_Alert := Low_Alert'(Empty_Alert with null record);

   Empty_Medium_Alert : Medium_Alert :=
     Medium_Alert'(Empty_Alert with Action_Officer => Volunteer);

   Empty_High_Alert : High_Alert :=
     High_Alert'(Empty_Medium_Alert with Ring_Alarm_At => Zero_Time);
end New_Alert_System.Objects;
