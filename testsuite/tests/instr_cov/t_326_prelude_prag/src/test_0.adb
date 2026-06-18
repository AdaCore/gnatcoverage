with Exemptions;
with Selective;

procedure Test_0 is
begin
   null;
end Test_0;

--# selective.ads
--
-- /cov/ lD ## dB:"Useless unit"

--# exemptions.adb
--
-- /exempt/    l* ## x+:"Useless unit"
-- /exempt_st/ l= ## Xs-
