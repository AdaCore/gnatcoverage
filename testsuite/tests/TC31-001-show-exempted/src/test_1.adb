with Exemptions;

procedure Test_1 is
   X : Positive := 1;
begin
   Exemptions (X);
   Exemptions (X);
end Test_1;

--# exemptions.adb
-- /if_1/            l+ ## 0
-- /inc_1/           l+ ## 0
-- /if_2/            l! ## dT-
-- /inc_2/           l- ## s-
-- /exempt1/         l* ## x+
-- /exempt2/         l* ## x+
