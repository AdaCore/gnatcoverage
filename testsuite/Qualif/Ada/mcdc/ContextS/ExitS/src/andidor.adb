package body Andidor is

   function F (A, B, C : Boolean) return Boolean is
   begin
      loop
         exit when A and then Identity (B or else C);  -- # evalStmt
         return False;                                 -- # decisionFalse
      end loop;
      return True;                                     -- # decisionTrue
   end;
end;
