pragma Ada_2012;
package Pkg2 is

   function Difficult_True (X : Integer) return Boolean is
     (if X mod 10 = 0 then (3 * X - 10) mod 10 = 0 else False);

   Val : Boolean := Difficult_True (30);

end Pkg2;
