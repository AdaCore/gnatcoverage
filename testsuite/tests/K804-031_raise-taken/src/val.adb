package body Val is

   --  Tests rely on the runtime check for the Id argument: we need a runtime
   --  check even when compiled with -gnatp.

   pragma Unsuppress (All_Checks);

   function GE0 (X : Num) return Boolean is
   begin
      if Id (X + 1) > 0 then -- # eval
         return True;   -- # true
      else
         return False;  -- # false
      end if;
   exception
      when Constraint_Error =>
         return X >= 0; -- # handler
   end;

end;
