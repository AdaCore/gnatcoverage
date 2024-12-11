pragma Ada_2012;

package Pak is

   type T is private;

   function Make (Cond : Boolean) return T;

private
   type T is tagged record
      X : Integer;
   end record;

   function Make (Cond : Boolean) return T
   is (T'(X => (if Cond then 1 else 2)));

   type TT is new T with record
      Y : Integer;
   end record;

   overriding function Make (Cond : Boolean) return TT
   is (TT'(X => 3, Y => 4));

end Pak;
