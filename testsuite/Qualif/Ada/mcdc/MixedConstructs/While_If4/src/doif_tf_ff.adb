with Support; use Support;
with Sensors, Slists.Fault; use Sensors, Slists, Slists.Fault;

procedure Doif_TF_FF is
   SL : Sensor_List;
   S : aliased Sensor;
begin
   S.Active := True;
   S.ALB := 1;
   S.AHB := 5;
   S.V   := 2;
   Prepend (S'Unchecked_Access, SL);

   declare
      Skip, Fault, Ok : Sensor_List;
   begin
      Control (SL, Active_Only => True,
               Skipped => Skip, Fault => Fault, Ok => Ok);
      Assert (Skip.Len = 0 and then Fault.Len = 0 and then Ok.Len = 1);
   end;
end;

