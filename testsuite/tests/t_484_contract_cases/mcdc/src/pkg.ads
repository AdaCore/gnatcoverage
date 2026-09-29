pragma Assertion_Policy (Check);
pragma Ada_2022;

package Pkg is
   function F (I : Integer) return Integer
   with Contract_Cases =>
     (I <= 0 or else I >= 10 =>        -- # guard-1
        F'Result <= 0 or else I >= 5,  -- # conseq-1
      others                 =>
        F'Result in 1 .. 5);           -- # conseq-2
end Pkg;
