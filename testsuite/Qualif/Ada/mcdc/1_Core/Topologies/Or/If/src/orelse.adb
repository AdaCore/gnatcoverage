package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
   begin
      if A or else B then  -- # evalStmt
         return True;      -- # decisionTrue
      else
         return False;     -- # decisionFalse
      end if;
   end;
end;

