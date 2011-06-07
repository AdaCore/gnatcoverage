with Passand, Silent_Last_Chance;

-- eval False only (from A), body stmt uncovered

procedure Test_Passand_FT is
begin
   Passand (False, True);
end;

--# passand.ads
--  /eval/ l! dT-

--# passand.adb
--  /eval/ l! dT-
--  /stmt/ l- s-
