pragma Ada_2012;

package Ops is
   function Bumpable (X : Integer) return Boolean
     with Ghost;

   procedure Bump (X : in out Integer)
     with Pre => Bumpable (X); -- # ghost-assert
end;
