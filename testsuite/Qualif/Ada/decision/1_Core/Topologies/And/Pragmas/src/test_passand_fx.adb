with Passand, Silent_Last_Chance;

-- eval False only (from A), body stmt uncovered

procedure Test_Passand_FX is
begin
   Passand (False, True);
end;

--# passand.ads
--  /eval/ l+ 0

--# passand.adb
--  /eval/ l+ 0
--  /stmt/ l- s-
