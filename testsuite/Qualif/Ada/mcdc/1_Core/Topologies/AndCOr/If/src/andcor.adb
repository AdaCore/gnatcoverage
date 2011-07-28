package body AndCOr is
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      if B or else C then -- # orelse :o/d:
	 return True;     -- # orTrue
      else
	 return False;    -- # orFalse
      end if;
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      if A and then Orelse (B, C) then -- # andthen :o/d:
         return True;                  -- # returnTrue
      else
         return False;                 -- # returnFalse
      end if;
   end;
end;
