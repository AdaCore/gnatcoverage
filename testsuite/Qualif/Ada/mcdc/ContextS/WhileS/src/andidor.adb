package body AndIdOr is

   function F (A, B, C : Boolean) return Boolean is
   begin
      while A and then Identity (B or else C) loop -- # evaluate
         return True;  -- # decisionTrue
      end loop;
      return False;    -- # decisionFalse
   end;
end;
