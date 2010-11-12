package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      for Value in (A and then B) .. True loop -- # evalStmt
         return Value;                         -- # returnValue
      end loop;
      raise Program_Error; -- should never reach here
   end;
end;
