--  Test driver for variant records. Its goal is to have a small part of
--  functional code covered. It creates only one record object, therefore only
--  one of record variants is chosen, and only those functional code that is
--  invoked by the initialization expression from this variant is expected to
--  be reported as covered.

with Check_Variants;
with Support; use Support;

procedure Test_Variants_Part_Small is
   Res   : Integer;
   Discr : Integer;
begin
   Discr := 1;
   Check_Variants (Res, Discr);
   Assert (Res = 1);

end Test_Variants_Part_Small;

--# variants_support.adb
--  /var1stmt/ l+ 0
--  /var2stmt/ l- s-
--  /var3stmt/ l- s-
--  /varothersstmt/ l- s-

-- # variant_3_g.ads
--  /var3gendcls/ l- s-

-- # variant_3_g.adb
--  /var3genstmts/ l- s-

-- # variant_others_g.ads
--  /varothersgendcls/ l- s-

-- # variant_others_g.adb
--  /varothersgenstmts/ l- s-
