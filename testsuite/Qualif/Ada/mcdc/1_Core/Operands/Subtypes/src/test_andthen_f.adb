with Support, Andthen_Variants; use Support, Andthen_Variants;

procedure Test_Andthen_F is
begin
   Assert (And_Then_Subtype (False, True) = False);
   Assert (And_Then_Subtype (True, False) = False);

   Assert (And_Then_Type (False, True) = False);
   Assert (And_Then_Type (True, False) = False);
end;

--# andthen_variants.adb
--  /evaluate/ l! ## eT-
