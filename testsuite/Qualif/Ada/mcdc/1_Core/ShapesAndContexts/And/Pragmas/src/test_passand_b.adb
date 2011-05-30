with Passand, Silent_Last_Chance;

-- eval both True and False, indep(B) only

procedure Test_Passand_B is
begin
   Passand (True, True);
   Passand (True, False);
end;

--# passand.ads
--  /eval/ l! c!:"A "

--# passand.adb
--  /eval/ l! c!:"A "
--  /stmt/ l+ 0
