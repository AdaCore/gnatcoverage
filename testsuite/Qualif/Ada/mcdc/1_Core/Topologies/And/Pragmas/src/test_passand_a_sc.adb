with Passand, Silent_Last_Chance;

-- eval both True and False, indep(A) demonstrated despite change on B,
-- shortcircuited

procedure Test_Passand_A_SC is
begin
   Passand (True, True);
   Passand (False, False);
end;

--# passand.ads
--  /eval/ l! c!:"B"

--# passand.adb
--  /eval/ l! c!:"B"
--  /stmt/ l+ 0
