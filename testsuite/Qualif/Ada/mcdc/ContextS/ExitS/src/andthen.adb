package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      loop
         exit when A and then B;  -- # evaluate
         return False;            -- # decisionFalse
      end loop;
      return True;                -- # decisionTrue
   end;
end;
