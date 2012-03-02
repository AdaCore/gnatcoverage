with Passand, Silent_Last_Chance;

-- eval both True and False, all stmt covered

procedure Test_Passand_FU is
begin
   Passand (True, True);
   Passand (True, False);
exception
    when others => null;
end;

--# passand.ads
--  /eval/ l+ 0

--# passand.adb
--  /eval/ l+ 0
--  /stmt/ l+ 0
