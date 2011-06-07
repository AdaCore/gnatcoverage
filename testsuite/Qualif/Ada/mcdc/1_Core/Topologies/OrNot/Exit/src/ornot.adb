package body Ornot is

   function F (A, B : Boolean) return Boolean is
   begin
      loop
         exit when A or else (not B);  -- # evalStmt
         return False;           -- # decisionFalse
      end loop;
      return True;               -- # decisionTrue
   end;
end;
