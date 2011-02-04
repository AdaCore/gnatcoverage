with Vbufs; use Vbufs;

package body Slists is

   procedure Prepend (S : Sensor_Access; SL : in out Sensor_List) is
      NA : Sensor_Node_Access;
   begin
      NA := new Sensor_Node'(S => S, Next => SL.Head);
      SL.Head := NA;
      SL.Len := SL.Len + 1;
   end;

end;
