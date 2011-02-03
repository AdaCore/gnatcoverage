
package Slists.Fault is
   procedure Control
     (SL : Sensor_List; Active_Only : Boolean;
      Skipped, Fault, Ok : out Sensor_List);
end;
