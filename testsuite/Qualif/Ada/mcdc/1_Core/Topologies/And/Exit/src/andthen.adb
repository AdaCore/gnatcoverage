package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      loop
         exit when A and then B;  -- # evalStmt :o/d:
         return False;            -- # decisionFalse
      end loop;
      return True;                -- # decisionTrue
   end;
end;
