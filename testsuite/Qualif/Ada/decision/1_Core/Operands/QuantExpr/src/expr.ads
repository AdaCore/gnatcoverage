pragma Ada_2012;
package Expr is
   type Sequence is array (Integer range <>) of Integer;

   type Predicate is access function (X : Integer) return Boolean;

   function GT_3 (X : Integer) return Boolean is (X > 3);
end;
