with Vbufs; use Vbufs;
with Simple_Pools;

package body Slists is
   
   package Sensor_Nodes_Pool is new
     Simple_Pools.Basic_Pool (Data_Type => Sensor_Node, Capacity => 20);
   
   procedure Prepend (S : Sensor_Access; SL : in out Sensor_List) is
      NA : constant Sensor_Nodes_Pool.Data_Access := Sensor_Nodes_Pool.Allocate;
   begin
      NA.all := (S => S, Next => SL.Head);
      SL := (Head => Sensor_Node_Access (NA), Len => SL.Len + 1);
   end;

end;
