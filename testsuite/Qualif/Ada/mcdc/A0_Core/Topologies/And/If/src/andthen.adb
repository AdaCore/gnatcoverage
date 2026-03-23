package body Andthen is
   function And_Then (A, B : Boolean) return Boolean is
   begin
      if A and then B then  -- # evalStmt :o/d:
         return True;       -- # decisionTrue
      else
         return False;      -- # decisionFalse
      end if;
   end;
end;
