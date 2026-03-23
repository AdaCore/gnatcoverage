package body Variant_3_G is

   procedure Adjust (I : in out Int) is
   begin
      I   := I + Tmp;     -- # var3genstmts
      Tmp := I;           -- # var3genstmts
   end Adjust;

end Variant_3_G;
