------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2025, AdaCore                     --
------------------------------------------------------------------------------

with Sensors;     use Sensors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Monitor is
   Sensor_Value : Integer;
begin
   for Sensor_Index in Sensor_Index_Range loop
      Sensor_Value := Sensors.Value (Sensor_Index);
      Put ("Sensor(" & Sensor_Index'Img & ") = " & Sensor_Value'Img & " ");
      if (Sensor_Value > 1000) then
         Put_Line ("!Alarm!");
      else
         Put_Line ("!Ok!");
      end if;
   end loop;
end;
