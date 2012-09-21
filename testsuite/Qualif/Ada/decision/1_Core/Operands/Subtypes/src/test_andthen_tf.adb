with Support, Andthen_Variants; use Support, Andthen_Variants;

procedure Test_Andthen_TF is
begin
   Assert (And_Then_Subtype (True, False) = False);
   Assert (And_Then_Type (True, False) = False);
end;

--# andthen_variants.adb
--  /eval/  l! ## dT-
--  /true/  l- ## s-
--  /false/ l+ ## 0
