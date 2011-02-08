package body Variant_Others_G is

   procedure Adjust (I : in out Int) is
   begin
      I   := I + Tmp;     -- # varothersgenstmts
      Tmp := I;           -- # varothersgenstmts
   end Adjust;

end Variant_Others_G;
