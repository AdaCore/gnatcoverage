package body Notor is

   function F (A, B : Boolean) return Boolean is
   begin
      loop
         exit when (not A) or else B;  -- # evalStmt :o/d:
         return False;           -- # decisionFalse
      end loop;
      return True;               -- # decisionTrue
   end;
end;
