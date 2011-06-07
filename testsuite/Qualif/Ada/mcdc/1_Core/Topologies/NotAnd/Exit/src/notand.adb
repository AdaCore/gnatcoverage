package body Notand is

   function F (A, B : Boolean) return Boolean is
   begin
      loop
         exit when (not A) and then B;  -- # evalStmt
         return False;            -- # decisionFalse
      end loop;
      return True;                -- # decisionTrue
   end;
end;
