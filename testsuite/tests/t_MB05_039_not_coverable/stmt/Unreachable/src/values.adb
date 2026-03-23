with State; use State;

package body Values is

   function Abs_Of (X : Integer) return Integer is

   begin
      if X > 0 then  -- # stmt
         return X;   -- # xpos
      else
         return -X;  -- # xneg
      end if;

      --  We always return prior to this point

      Dtick; -- # out
   end;

end;
