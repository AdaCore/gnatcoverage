--  test for (A or else B) and then (C or else D)

package body PorPandPorP is

   function F (A, B, C, D : Boolean) return Boolean
   is
   begin
      if (A or else B) and then (C or else D) then -- # evalStmt
         return True; -- # decisionTrue
      else
         return False; -- # decisionFalse
      end if;
   end F;

end PorPandPorP;
