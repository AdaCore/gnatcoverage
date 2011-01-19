package body Notor is

   function F (A, B : Boolean) return Boolean is
   begin
      while (not A) or else B loop -- # evalStmt
         return True;        -- # decisionTrue
      end loop;
      return False;          -- # decisionFalse
   end;
end;
