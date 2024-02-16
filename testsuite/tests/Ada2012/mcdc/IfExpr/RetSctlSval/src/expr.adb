pragma Ada_2012;

package body Expr is

   function Filter (A, B : Integer) return Boolean is
   begin
      return (if (A > 0)    -- # ctl-apos
                then B > A  -- # then-bgt
                else B < A  -- # else-agt
             );
   end;
end;
