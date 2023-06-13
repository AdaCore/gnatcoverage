package body Temperature_Control is

   Ticks : Integer := 0;
   Inc_Period : Integer := 5;

   Live_Temperature : T := 0;

   procedure Update_Live_Temperature is
   begin
      Ticks := Ticks + 1;
      if Ticks mod Inc_Period = 0 then
         Live_Temperature := Live_Temperature + 1;
      end if;
   end;

   function Current_Temperature return T is
   begin
      Update_Live_Temperature;
      return Live_Temperature;
   end;

end;
