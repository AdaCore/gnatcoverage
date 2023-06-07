

-- We happen to use the Math package for basic operations here,
-- but we're not part of the unit tests for this unit.

with Math; use Math;

package body Maps is
   
   function Area (R : Rectangle_T) return Natural is
   begin
      return Eval (Mult, R.L, R.W); -- # eval
   end;
end;
