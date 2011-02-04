with Support; use Support;
with Sensors, Slists.Fault; use Sensors, Slists, Slists.Fault;

procedure Doif_FX_FT is
   SL : Sensor_List;
   S : aliased Sensor;
begin
   S.Active := True;
   S.ALB := 1;
   S.AHB := 5;
   S.V   := 25;
   Prepend (S'Unchecked_Access, SL);

   declare
      Skip, Fault, Ok : Sensor_List;
   begin
      Control (SL, Active_Only => False,
               Skipped => Skip, Fault => Fault, Ok => Ok);
      Assert (Skip.Len = 0 and then Fault.Len = 1 and then Ok.Len = 0);
   end;
end;

