--  Common bits for providers of functional decision evaluators (fuor etc)

package Sensors is

   --  Fake model of a sensor with a current value and a lo .. hi
   --  stability range

   type Sensor is record
      V, Lo, Hi : Integer;
   end record;

   type Sensor_Pair is record
      A, B : Sensor;
   end record;

   OK : constant Sensor := (V => 2, Lo => 1, Hi => 3);
   NOK : constant Sensor := (V => 0, Lo => 2, Hi => 4);

end;
