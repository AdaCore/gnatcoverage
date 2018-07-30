with Support; use Support;

package body Values is
   procedure Touch (E : in out Num) is
   begin
      E := E * 2;
   end;
   
   procedure Check_Value (E : Num; V : Integer) is
   begin
      Assert (Integer(E) = V);
   end;
end;
