package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      for Value in (A or else B) .. True loop -- # evalStmt
         return Value;                        -- # returnValue
      end loop;
      raise Program_Error; -- should never reach here
   end;
end;
