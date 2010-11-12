package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      while A or else B loop -- # evalStmt
         return True;        -- # decisionTrue
      end loop;
      return False;          -- # decisionFalse
   end;
end;
