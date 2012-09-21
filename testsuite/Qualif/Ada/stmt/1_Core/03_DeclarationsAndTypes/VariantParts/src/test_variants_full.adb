--  Test driver for variant records. Its goal is to have all the functional
--  code covered. It creates objects that explore all the variants defined for
--  the record type, therefore all the functional code is expected to be
--  reported as covered.

with Check_Variants;
with Support; use Support;

procedure Test_Variants_Full is
   Res   : Integer;
   Discr : Integer;
begin
   Discr := 1;
   Check_Variants (Res, Discr);
   Assert (Res = 1);

   Discr := 2;
   Check_Variants (Res, Discr);
   Assert (Res = 2);

   Discr := 3;
   Check_Variants (Res, Discr);
   Assert (Res = 6);

   Discr := 4;
   Check_Variants (Res, Discr);
   Assert (Res = 8);
end Test_Variants_Full;

--# variants_support.adb
--  /var1stmt/ l+ ## 0
--  /var2stmt/ l+ ## 0
--  /var3stmt/ l+ ## 0
--  /varothersstmt/ l+ ## 0

-- # variant_3_g.ads
--  /var3gendcls/ l+ ## 0

-- # variant_3_g.adb
--  /var3genstmts/ l+ ## 0

-- # variant_others_g.ads
--  /varothersgendcls/ l+ ## 0

-- # variant_others_g.adb
--  /varothersgenstmts/ l+ ## 0
