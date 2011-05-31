package body Andthen is
   function And_Then (A, B : Boolean) return Boolean is
   begin
      if A and then B then  -- # andthen
         return True;       -- # retTrue
      end if;
      return False;      -- # retFalse
   end;
end;



