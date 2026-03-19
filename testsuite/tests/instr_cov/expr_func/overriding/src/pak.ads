pragma Ada_2012;

package Pak is

   type T is record
      X : Integer;
   end record;
   function Make (Cond : Boolean) return T is (T'(X => 1));

   type TT is new T;
   overriding function Make (Cond : Boolean) return TT is
     (TT'(X => (if Cond then 2 else 3)));  -- # dc

end Pak;
