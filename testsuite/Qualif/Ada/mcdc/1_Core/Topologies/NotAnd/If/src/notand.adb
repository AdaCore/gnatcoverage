package body Notand is
   function F (A, B : Boolean) return Boolean is
   begin
      if (not A) and then B then  -- # evalStmt
         return True;       -- # decisionTrue
      else
         return False;      -- # decisionFalse
      end if;
   end;
end;
