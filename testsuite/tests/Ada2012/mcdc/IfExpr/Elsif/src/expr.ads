pragma Ada_2012;

package Expr is
   X : Boolean := True; -- arbitrary value for operand not evaluated

   function Filter (A, B : Boolean; Valt : Boolean) return Boolean is
      ((if A then Valt elsif B then Valt else not Valt)); -- # eval

      --  The controlling expressions are mono-operand here, and we
      --  have no dominated statement to disambiguate True from False
      --  on partial coverage.
end;
