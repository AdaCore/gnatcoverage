with Variants; use Variants;
procedure Check_Variants
  (Res   : out Integer;
   Discr : in Integer)
is
   Tmp : Variant_Record (D => Discr);
begin
   case Discr is
      when 1 =>
         Res := Tmp.C1;
      when 2 =>
         Res := Tmp.C2;
      when 3 =>
         Res := Tmp.C3;
      when others =>
         Res := Tmp.C_Others;
   end case;
end Check_Variants;
