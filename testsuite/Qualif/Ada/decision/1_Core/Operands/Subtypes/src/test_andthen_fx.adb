with Support, Andthen_Variants; use Support, Andthen_Variants;

procedure Test_Andthen_FX is
begin
   for X in False .. True loop
      Assert (And_Then_Subtype (False, X) = False);
      Assert (And_Then_Type (False, Bool_Type(X)) = False);
   end loop;
end;

--# andthen_variants.adb
--  /eval/  l! ## dT-
--  /true/  l- ## s-
--  /false/ l+ ## 0
