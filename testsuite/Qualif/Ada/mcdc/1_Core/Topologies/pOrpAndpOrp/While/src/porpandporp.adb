package body PorPandPorP is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      while (A or else B) and then (C or else D) loop -- # evalStmt :o/d:
         return True;                     -- # decisionTrue
      end loop;
      return False;                       -- # decisionFalse
   end;
end;
