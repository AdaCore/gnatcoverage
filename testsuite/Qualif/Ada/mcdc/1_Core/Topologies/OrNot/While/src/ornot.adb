package body Ornot is

   function F (A, B : Boolean) return Boolean is
   begin
      while A or else (not B) loop -- # evalStmt :o/d:
         return True;        -- # decisionTrue
      end loop;
      return False;          -- # decisionFalse
   end;
end;
