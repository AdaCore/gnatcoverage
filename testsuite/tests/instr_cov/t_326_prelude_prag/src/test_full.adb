with Exemptions;
with Selective;

procedure Test_Full is
begin
   Exemptions.Foo;
   Selective.Bar;
end Test_Full;

--# selective.ads
--
-- /cov/ lD ## dB:"Useless unit"

--# exemptions.adb
--
-- /exempt/ l# ## x0:"Useless unit"
