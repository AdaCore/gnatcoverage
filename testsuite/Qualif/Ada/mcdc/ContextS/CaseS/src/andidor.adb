package body AndIdOr is

   function F (A, B, C : Boolean) return Boolean is
   begin
      case A and then Identity (B or else C) is -- # evalStmt
         when True =>
            return True;               -- # decisionTrue
         when False =>
            return False;              -- # decisionFalse
      end case;
   end;

end;
