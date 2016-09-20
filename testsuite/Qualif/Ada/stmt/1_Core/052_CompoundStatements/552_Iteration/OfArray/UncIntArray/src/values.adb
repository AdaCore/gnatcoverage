with Support; use Support;

package body Values is
   procedure Touch (E : in out Integer) is
   begin
      E := E * 2;
   end;
   
   procedure Check_Value (E : Integer; V : Integer) is
   begin
      Assert (E = V);
   end;
end;
