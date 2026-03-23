package body Notornot is

   function F (A, B : Boolean) return Boolean is
   begin
      while (not A) or else (not B) loop -- # evalStmt :o/d:
         return True;        -- # decisionTrue
      end loop;
      return False;          -- # decisionFalse
   end;
end;
