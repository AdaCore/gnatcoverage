
package body Sensors is
   procedure Sample (S: in out Sensor) is
   begin
      Vbufs.Push (S.V, S.History);
   end;
end;


