package body AndIdOr is
   function F (A, B, C : Boolean) return Boolean is
   begin
      for V in (A and then Identity (B or else C)) .. True loop -- # evaluate
         return V;         -- # returnValue
      end loop;
      raise Program_Error; -- should never reach here
   end;
end;
