package Slists.Forall is
   type Sensor_Op is (Activate, Inhibit);
   procedure ForAll_In
     (SL : Sensor_List; Op : Sensor_Op; Active_Only : Boolean := False);
end;

