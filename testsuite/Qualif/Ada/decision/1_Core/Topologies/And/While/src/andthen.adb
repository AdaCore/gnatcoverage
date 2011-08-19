package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      while A and then B loop  -- # andthen :o/d:
         return True;          -- # retTrue
      end loop;
      return False;      -- # retFalse
   end;
end;



