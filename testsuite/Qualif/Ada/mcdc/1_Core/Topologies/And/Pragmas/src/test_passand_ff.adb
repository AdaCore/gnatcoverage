with Passand, Silent_Last_Chance;

-- eval False only, body stmt uncovered

procedure Test_Passand_FF is
begin
   Passand (False, False);
end;

--# passand.ads
--  /eval/ l! dT-

--# passand.adb
--  /eval/ l! dT-
--  /stmt/ l- s-
