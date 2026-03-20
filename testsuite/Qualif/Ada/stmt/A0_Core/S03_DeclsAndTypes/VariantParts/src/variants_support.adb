with Variant_3_G;
with Variant_Others_G;
package body Variants_Support is

   function F1 (I : Integer) return Integer is
   begin
      return I;              -- # var1stmt
   end F1;

   function F2 (I : Integer) return Integer is
   begin
      return I;              -- # var2stmt
   end F2;

   function F3 (I : Integer) return Integer is
      package Variant_3 is new Variant_3_G (Integer, I);  -- # var3stmt
      Tmp : Integer := I;                                 -- # var3stmt
   begin
      Variant_3.Adjust (Tmp);  -- # var3stmt
      return Tmp;              -- # var3stmt
   end F3;

   function F_Others (I : Integer) return Integer is
      package Variant_Others is new Variant_Others_G (Integer, I); -- # varothersstmt
      Tmp : Integer := I;                                          -- # varothersstmt
   begin
      Variant_Others.Adjust (Tmp);  -- # varothersstmt
      return Tmp;                   -- # varothersstmt
   end F_Others;

end Variants_Support;
