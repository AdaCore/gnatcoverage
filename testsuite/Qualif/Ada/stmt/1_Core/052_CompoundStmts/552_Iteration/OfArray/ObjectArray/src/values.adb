with Support; use Support;

package body Values is
   
   procedure Touch (E : in out Object) is
   begin
      E.X := E.X * 2;
   end;
      
   procedure Check_Value (E : Object; V : Integer) is
   begin
      Assert (E.X = V);
   end;
end;
