pragma Ada_2012;

package Pkg is

   type T is tagged record
      X : Boolean;
   end record;

   function Store_AT (L : Boolean; R : Boolean) return T is
     (X => L and then R);

end Pkg;
