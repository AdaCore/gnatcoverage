generic
   type Int is range <>;
   Init_Value : Int;
package Variant_3_G is
   Tmp : Int := Init_Value;           -- # var3gendcls

   procedure Adjust (I : in out Int);

end Variant_3_G;
