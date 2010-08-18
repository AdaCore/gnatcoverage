package body PorPandPorP is

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      case (A or else B) and then (C or else D) is -- # evaluate
         when True =>
            return True;               -- # decisionTrue
         when False =>
            return False;              -- # decisionFalse
      end case;
   end;

end;
