with Passand;

-- eval True only, all stmts covered

procedure Test_Passand_TT is
begin
   Passand (True, True);
end;

--# passand.ads
--  /eval/ l+ ## 0

--# passand.adb
--  /eval/ l+ ## 0
--  /stmt/ l+ ## 0
