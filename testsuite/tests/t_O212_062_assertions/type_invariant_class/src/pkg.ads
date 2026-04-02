pragma Ada_2022;
pragma Assertion_Policy (Check);

package Pkg is

   type Non_Zero (I : Integer) is tagged private      -- # non_zero_decl
   with Type_Invariant'Class => I /= 0;               -- # type_class_inv

   type Sub_Non_Zero is new Non_Zero with private;    -- # child_rec_decl
   function Make_Rec (I : Integer) return Sub_Non_Zero;

private

   type Non_Zero (I : Integer) is tagged null record
   with Type_Invariant => I > 0;                      -- # type_inv
   --  Type_Invariant on the Parent type is not triggered by Child

   type Sub_Non_Zero is new Non_Zero with null record;

   function Make_Rec (I : Integer) return Sub_Non_Zero is ((I => I)); -- # return

end Pkg;
