package body PorPandPorP is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      loop
         exit when (A or else B) and then  (C or else D);  -- # evalStmt :o/d:
         return False;                        -- # decisionFalse
      end loop;
      return True;                            -- # decisionTrue
   end;
end;
