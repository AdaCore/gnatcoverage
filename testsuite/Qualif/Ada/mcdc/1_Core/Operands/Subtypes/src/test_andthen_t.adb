with Support, Andthen_Variants; use Support, Andthen_Variants;

procedure Test_Andthen_T is
begin
   Assert (And_Then_Subtype (True, True) = True);

   Assert (And_Then_Type (True, True) = True);
end;

--# andthen_variants.adb
--  /evaluate/ l! eF-
