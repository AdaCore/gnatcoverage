pragma Ada_2012;

package Pak is
   type T is tagged record
      X : Integer;
   end record;
   function And_Then (X : T; A, B : Boolean) return Boolean is
      (A and then B);
   function Or_Else (X : T; A, B : Boolean) return Boolean is
      (A or else B);

   function Make_Internal (Cond : Boolean) return T;

   function Make (Cond : Boolean) return T is (Make_Internal (Cond));

   type TT is new T with record
      Y : Integer;
   end record;

   overriding function Make_Internal (Cond : Boolean) return TT;

   overriding function Make (Cond : Boolean) return TT is
      (Make_Internal (Cond));

end Pak;
