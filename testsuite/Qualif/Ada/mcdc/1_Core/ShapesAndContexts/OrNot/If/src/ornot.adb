package body Ornot is
   function F (A, B : Boolean) return Boolean is
   begin
      if A or else (not B) then  -- # evalStmt
         return True;      -- # decisionTrue
      else
         return False;     -- # decisionFalse
      end if;
   end;
end;

