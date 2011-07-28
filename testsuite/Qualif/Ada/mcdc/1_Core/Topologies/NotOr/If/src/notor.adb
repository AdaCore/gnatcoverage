package body Notor is
   function F (A, B : Boolean) return Boolean is
   begin
      if (not A) or else B then  -- # evalStmt :o/d:
         return True;      -- # decisionTrue
      else
         return False;     -- # decisionFalse
      end if;
   end;
end;

