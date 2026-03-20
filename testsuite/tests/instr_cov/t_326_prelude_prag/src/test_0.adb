with Exemptions;
with Selective;

procedure Test_0 is
begin
   null;
end Test_0;

--# selective.ads
--
-- /cov/ lD ## dB

--# exemptions.adb
--
-- /exempt/    l* ## x+
-- /exempt_st/ l= ## Xs-
