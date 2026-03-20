with Passand, Silent_Last_Chance;

-- eval both True and False, indep(A) only

procedure Test_Passand_A is
begin
   Passand (True, True);
   Passand (False, True);
exception
    when others => null;
end;

--# passand.ads
--  /eval/ l! ## c!:"B"

--# passand.adb
--  /eval/ l! ## c!:"B"
--  /stmt/ l+ ## 0
