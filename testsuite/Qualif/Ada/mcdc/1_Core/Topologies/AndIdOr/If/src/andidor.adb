package body AndIdOr is
   function F (A, B, C : Boolean) return Boolean is
   begin
      if A and then Identity (B or else C) then -- # evalStmt :o/d:
         return True;                           -- # decisionTrue
      else
         return False;                          -- # decisionFalse
      end if;
   end;
end;
