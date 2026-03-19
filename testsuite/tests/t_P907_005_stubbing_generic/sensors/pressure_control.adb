package body Pressure_Control is

   Ticks : Integer := 0;
   Inc_Period : Integer := 3;

   Live_Pressure : T := 6;

   procedure Update_Live_Pressure is
   begin
      Ticks := Ticks + 1;
      if Ticks mod Inc_Period = 0 then
         Live_Pressure := Live_Pressure + 1;
      end if;
   end;

   function Current_Pressure return T is
   begin
      Update_Live_Pressure;
      return Live_Pressure;
   end;

end;
