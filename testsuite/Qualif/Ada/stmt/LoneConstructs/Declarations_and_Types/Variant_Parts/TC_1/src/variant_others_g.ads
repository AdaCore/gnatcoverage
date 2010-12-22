generic
   type Int is range <>;
   Init_Value : Int;
package Variant_Others_G is
   Tmp : Int := Init_Value;           -- # varothersgendcls

   procedure Adjust (I : in out Int);

end Variant_Others_G;
