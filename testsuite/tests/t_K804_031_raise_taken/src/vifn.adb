package body Vifn is

   --  Tests rely on the runtime check for the Id argument: we need a runtime
   --  check even when compiled with -gnatp.

   pragma Unsuppress (All_Checks);

   procedure Check_GE0 (X : Num) is
   begin
      if not (Id (X + 1) <= 0) then   -- # eval
         N_Ge0 := N_Ge0 + 1;   -- # true
      end if;
   exception
      when Constraint_Error =>
         N_Check_Raise := N_Check_Raise + 1;  -- # handler
   end;

end;
