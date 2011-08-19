package body Andthen is
   function And_Then (A, B : Boolean) return Boolean is
   begin
      if A and then B then  -- # andthen :o/d:
         return True;       -- # retTrue
      else
         return False;      -- # retFalse
      end if;
   end;
end;



