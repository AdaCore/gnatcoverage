with Support, Andthen_Variants ; use Support, Andthen_Variants;

procedure Test_Andthen_AB is
begin
   Assert (And_Then_Subtype (True, True) = True);
   Assert (And_Then_Subtype (True, False) = False);
   Assert (And_Then_Subtype (False, True) = False);

   Assert (And_Then_Type (True, True) = True);
   Assert (And_Then_Type (True, False) = False);
   Assert (And_Then_Type (False, True) = False);
end;

--# andthen_variants.adb
--  /evaluate/ l+ 0
