with Sensors; use Sensors;

package Slists is

   type Sensor_Node;
   type Sensor_Node_Access is access all Sensor_Node;

   type Sensor_Node is record
      S : Sensor_Access;
      Next : Sensor_Node_Access;
   end record;

   type Sensor_List is record
      Head : Sensor_Node_Access := null;
      Len  : Natural := 0;
   end record;

   procedure Prepend (S : Sensor_Access; SL : in out Sensor_List);

end Slists;
