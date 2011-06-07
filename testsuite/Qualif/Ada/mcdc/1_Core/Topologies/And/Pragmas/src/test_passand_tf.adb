with Passand, Silent_Last_Chance;

-- eval False only (from B), body stmt uncovered

procedure Test_Passand_TF is
begin
   Passand (True, False);
end;

--# passand.ads
--  /eval/ l! dT-

--# passand.adb
--  /eval/ l! dT-
--  /stmt/ l- s-
