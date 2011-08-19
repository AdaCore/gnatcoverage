with Passand, Silent_Last_Chance;

-- eval False only (from B), body stmt uncovered

procedure Test_Passand_TF is
begin
   Passand (True, False);
end;

--# passand.ads
--  /eval/ l+ 0

--# passand.adb
--  /eval/ l+ 0
--  /stmt/ l- s-
