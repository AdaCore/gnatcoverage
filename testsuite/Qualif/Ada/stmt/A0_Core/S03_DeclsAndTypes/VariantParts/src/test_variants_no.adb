--  Test driver for variant records. It only (indirectly) "with"s the
--  functional code, but does not execute anything from it, so nothing from the
--  constructs of interest is expected to be reported as covered.

with Check_Variants;
with Support; use Support;

procedure Test_Variants_No is
begin
   Assert (True);
end Test_Variants_No;

--# variants_support.adb
--  /var1stmt/ l- ## s-
--  /var2stmt/ l- ## s-
--  /var3stmt/ l- ## s-
--  /varothersstmt/ l- ## s-

-- # variant_3_g.ads
--  /var3gendcls/ l- ## s-

-- # variant_3_g.adb
--  /var3genstmts/ l- ## s-

-- # variant_others_g.ads
--  /varothersgendcls/ l- ## s-

-- # variant_others_g.adb
--  /varothersgenstmts/ l- ## s-
