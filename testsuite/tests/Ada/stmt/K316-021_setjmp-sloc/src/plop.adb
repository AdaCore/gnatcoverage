package body Plop is
   pragma Unsuppress (All_Checks);

   function F (I : My_Int) return My_Int is
      Res : My_Int := I;
   begin
      begin
         Res := Res * I;          -- # mult
         Res := Res / 2;          -- # div
      exception
         when Constraint_Error =>
            return -1;            -- # handle
      end;

      return Res;                 -- # retval
   end;
end;
