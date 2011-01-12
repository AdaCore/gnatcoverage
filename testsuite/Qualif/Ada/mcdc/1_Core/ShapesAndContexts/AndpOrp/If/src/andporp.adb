package body AndPorP is

   function F (A, B, C : Boolean) return Boolean is
   begin
      if A and then (B or else C) then -- # evalStmt
         return True;                  -- # decisionTrue
      else
         return False;                 -- # decisionFalse
      end if;
   end F;

end AndPorP;
