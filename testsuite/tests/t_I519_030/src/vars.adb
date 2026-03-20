package body Vars is
   procedure Assert_Eq (Value, Expected : Integer) is
   begin
      if Value /= Expected then
         raise Program_Error;
      end if;
   end;
end;
