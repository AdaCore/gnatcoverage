with Vbufs; use Vbufs;
with Simple_Pools;

package body Slists is
   
   package Sensor_Nodes_Pool is 
      new Simple_Pools.Basic_Pool (Data_Type => Sensor_Node,
                                   Data_Access => Sensor_Node_Access,
                                   Capacity => 20);
   
   procedure Prepend (S : Sensor_Access; SL : in out Sensor_List) is
      NA : constant Sensor_Node_Access  := Sensor_Nodes_Pool.Allocate;
   begin
      NA.S := S;
      NA.Next := SL.Head;
      SL.Head := NA;
      SL.Len := SL.Len + 1;
   end;

end;
