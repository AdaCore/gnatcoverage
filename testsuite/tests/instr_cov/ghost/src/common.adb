package body Common is
   procedure Check_Pos (X : Integer) is
   begin
      if X <= 0 then -- # check
         raise Program_Error; -- # raise
      end if;
   end;
end;
