package body Ornot is

   function F (A, B : Boolean) return Boolean is
   begin
      case A or else (not B) is -- # evalStmt :o/e:
         when True =>
            return True;    -- # decisionTrue
         when False =>
            return False;   -- # decisionFalse
      end case;
   end;

end;
