package body AndCOr is
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      return B or else C; -- # orelse
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      for V in (A and then Orelse (B, C)) .. True loop -- # andthen
	 return V;         -- # returnValue
      end loop;
      raise Program_Error; -- should never reach here
   end;
end;
