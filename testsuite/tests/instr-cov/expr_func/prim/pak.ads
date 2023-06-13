pragma Ada_2012;

package Pak is
   type T is tagged record
      X : Integer;
   end record;
   function And_Then (X : T; A, B : Boolean) return Boolean is
      (A and then B);
   function Or_Else (X : T; A, B : Boolean) return Boolean is
      (A or else B);
   function Make (Cond : Boolean) return T is (T'(X => (if Cond then 1 else 2)));

   type TT is new T with record
      Y : Integer;
   end record;
   overriding function Make (Cond : Boolean) return TT is (TT'(X => 3, Y => 4));

end Pak;
