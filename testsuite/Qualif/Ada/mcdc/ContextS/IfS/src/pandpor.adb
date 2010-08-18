package body PandPor is
   function F (A, B, C : Boolean) return Boolean is
   begin
      if (A and then B) or else C then  -- # evaluate
         return True;                   -- # decisionTrue
      else
         return False;                  -- # decisionFalse
      end if;
   end;
end;
