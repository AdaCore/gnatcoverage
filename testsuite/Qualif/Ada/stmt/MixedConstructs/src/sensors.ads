with Vbufs; use Vbufs;

package Sensors is

   type Sensor (Hist_Size : Natural) Is record
      V, ALB, AHB : Value;
      Active : Boolean := False;
      History : Vbuffer (Size => Hist_Size);
   end record;

   type Sensor_Access is access all Sensor;

   procedure Sample (S: in out Sensor);
   -- latch current sensor value for S in its history buffer

end;

