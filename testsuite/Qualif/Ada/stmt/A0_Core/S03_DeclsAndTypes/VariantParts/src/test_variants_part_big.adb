--  Test driver for variant records. Its goal is to have a big part of
--  functional code covered. It creates objects that explore three from four
--  variants defined for the record type, therefore all the functional code
--  that is invoked by the initialization expressions from these variants is
--  expected to be reported as covered.

with Check_Variants;
with Support; use Support;

procedure Test_Variants_Part_Big is
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

end Test_Variants_Part_Big;

--# variants_support.adb
--  /var1stmt/ l+ ## 0
--  /var2stmt/ l+ ## 0
--  /var3stmt/ l+ ## 0
--  /varothersstmt/ l- ## s-

-- # variant_3_g.ads
--  /var3gendcls/ l+ ## 0

-- # variant_3_g.adb
--  /var3genstmts/ l+ ## 0

-- # variant_others_g.ads
--  /varothersgendcls/ l- ## s-

-- # variant_others_g.adb
--  /varothersgenstmts/ l- ## s-
