package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      while A and then B loop -- # evaluate
         return True;         -- # decisionTrue
      end loop;
      return False;           -- # decisionFalse
   end;
end;
