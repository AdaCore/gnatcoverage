pragma Ada_2012;

package body Pak is

   type BT is tagged record
      X : Integer;
   end record;

   function Make (Cond : Boolean) return BT
   is (BT'(X => (if Cond then 1 else 2)));

   type BTT is new BT with record
      Y : Integer;
   end record;

   function Make (Cond : Boolean) return BTT
   is (BTT'(X => 3, Y => 4));

   function Exercise_BT (Cond : Boolean) return Integer
   is (BT'(Make (Cond)).X);

   function Exercise_BTT (Cond : Boolean) return Integer
   is (BTT'(Make (Cond)).Y);

end Pak;
