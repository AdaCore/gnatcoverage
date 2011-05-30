with Vbufs; use Vbufs;

package Sensors is

   subtype Hist_Index is Natural range 0 .. 20;

   type Sensor (Hist_Size : Hist_Index := 5) Is record
      V, ALB, AHB : Value;
      Active : Boolean := False;
      History : Vbuffer (Size => Hist_Size);
   end record;

   type Sensor_Access is access all Sensor;

   procedure Sample (S: in out Sensor);
   -- latch current sensor value for S in its history buffer

end;

