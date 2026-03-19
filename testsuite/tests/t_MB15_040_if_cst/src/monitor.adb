
package body Monitor is

   Moncalls : Natural := 0;
   pragma Volatile (Moncalls);

   procedure Op is
   begin
      if Count_Ops then          -- # called
         Opcount := Opcount + 1; -- # cond-stmt
      end if;
      Moncalls := Moncalls + 1;  -- # called
   end;
end;
