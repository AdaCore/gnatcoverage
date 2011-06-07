package body Notandnot is

   function F (A, B : Boolean) return Boolean is
   begin
      while (not A) and then (not B) loop -- # evalStmt
         return True;         -- # decisionTrue
      end loop;
      return False;           -- # decisionFalse
   end;
end;
