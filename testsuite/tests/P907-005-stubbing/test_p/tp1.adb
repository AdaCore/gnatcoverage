with Temperature_Control; use Temperature_Control;
with Pressure_Control; use Pressure_Control;

procedure TP1 is
   T : Integer;
begin
   T := Current_Temperature;
   pragma Assert (T < 0);
   T := Current_Pressure;
   pragma Assert (T >= 0);
end;
