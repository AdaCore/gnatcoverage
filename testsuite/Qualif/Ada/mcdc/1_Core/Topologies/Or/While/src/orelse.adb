package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      while A or else B loop -- # evalStmt :o/d:
         return True;        -- # decisionTrue
      end loop;
      return False;          -- # decisionFalse
   end;
end;
