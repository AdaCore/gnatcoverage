package body Pkg is
   procedure Check (X : Integer) is
   begin
      if X < 0 then
         raise Program_Error;
      end if;
   end Check;
end Pkg;
