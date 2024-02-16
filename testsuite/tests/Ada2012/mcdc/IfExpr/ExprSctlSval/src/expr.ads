pragma Ada_2012;

package Expr is

   --  Simple controlling expression, simple value expressions,
   --  as expression function.

   function Filter (A, B : Integer) return Boolean is
      (if (A > 0)    -- # ctl-apos
         then B > A  -- # then-bgt
         else B < A  -- # else-agt
      );

end;
