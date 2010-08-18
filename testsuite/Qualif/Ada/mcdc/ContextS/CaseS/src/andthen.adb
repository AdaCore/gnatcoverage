package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      case A and then B is -- # evaluate
         when True =>
            return True;   -- # decisionTrue
         when False =>
            return False;  -- # decisionFalse
      end case;
   end;
end;
