with Support, Andthen_Variants; use Support, Andthen_Variants;

procedure Test_Andthen_B is
begin
   Assert (And_Then_Subtype (True, True) = True);
   Assert (And_Then_Subtype (True, False) = False);

   Assert (And_Then_Type (True, True) = True);
   Assert (And_Then_Type (True, False) = False);
end;

--# andthen_variants.adb
--  /evaluate/ l! m!:"A"
