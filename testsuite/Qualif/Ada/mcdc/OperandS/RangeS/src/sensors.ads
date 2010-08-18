package Sensors is
   type Sensor is record
      V, Lo, Hi : Integer;
   end record;
   
   OK : constant Sensor := (V => 2, Lo => 1, Hi => 3);
   NOK : constant Sensor := (V => 0, Lo => 2, Hi => 4);
   
   function Both_OK (A, B : Sensor) return Boolean;
end;
