package body AndPorP is

   function F (A, B, C : Boolean) return Boolean is
   begin
      case A and then (B or else C) is -- # evaluate
         when True =>
            return True;               -- # decisionTrue
         when False =>
            return False;              -- # decisionFalse
      end case;
   end;

end;
