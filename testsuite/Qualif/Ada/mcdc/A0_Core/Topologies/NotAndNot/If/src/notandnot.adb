package body Notandnot is
   function F (A, B : Boolean) return Boolean is
   begin
      if (not A) and then (not B) then  -- # evalStmt :o/d:
         return True;       -- # decisionTrue
      else
         return False;      -- # decisionFalse
      end if;
   end;
end;
