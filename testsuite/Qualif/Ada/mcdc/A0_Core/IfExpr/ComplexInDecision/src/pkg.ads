pragma Ada_2012;

package Pkg is

   function P
     (A : Boolean;
      B : Boolean;
      C : Boolean;
      D : Boolean;
      E : Boolean;
      F : Boolean) return Boolean
   is
      ((if A                  -- # eval-if-expr
          then B and then C   -- # decision-1-if-expr
          else D and then E)  -- # decision-2-if-expr
       or else F);            -- # last-cond

end Pkg;
