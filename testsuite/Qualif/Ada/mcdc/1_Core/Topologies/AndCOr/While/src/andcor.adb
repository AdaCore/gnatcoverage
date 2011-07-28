package body AndCor is
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      while B or else C loop -- # orelse :o/d:
	 return True;  -- # orTrue
      end loop;
      return False;    -- # orFalse
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      while A and then Orelse (B, C) loop -- # andthen :o/d:
         return True;  -- # returnTrue
      end loop;
      return False;    -- # returnFalse
   end;
end;
