package body PandPor is

   function F (A, B, C : Boolean) return Boolean is
   begin
      for Value in ((A and then B) or else C) .. True loop -- # evalStmt
         return Value;                                     -- # returnValue
      end loop;
      raise Program_Error; -- should never reach here
   end;
end;
