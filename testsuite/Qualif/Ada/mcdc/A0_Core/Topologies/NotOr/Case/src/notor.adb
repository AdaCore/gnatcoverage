package body Notor is

   function F (A, B : Boolean) return Boolean is
   begin
      case (not A) or else B is -- # evalStmt :o/e:
         when True =>
            return True;    -- # decisionTrue
         when False =>
            return False;   -- # decisionFalse
      end case;
   end;

end;
