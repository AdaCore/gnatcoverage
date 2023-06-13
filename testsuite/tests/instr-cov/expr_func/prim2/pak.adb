pragma Ada_2012;

package body Pak is

   function Make_Internal (Cond : Boolean) return T is
      (T'(X => (if Cond then 1 else 2)));

   function Make_Internal (Cond : Boolean) return TT is (TT'(X => 3, Y => 4));

end Pak;