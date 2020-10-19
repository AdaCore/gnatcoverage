with Values.Aux;
package body Values is
   procedure Scan (X : Integer) is
   begin
      if X < 0 then
         N_LT0 := N_LT0 + 1;
      else
         N_GE0 := N_GE0 + 1;
      end if;
   end;
end;
