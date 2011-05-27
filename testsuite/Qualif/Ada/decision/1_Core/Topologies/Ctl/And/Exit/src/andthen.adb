package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      loop
         exit when A and then B; -- # andthen
         return False;           -- # retFalse
      end loop;
      return True;      -- # retTrue
   end;
end;
