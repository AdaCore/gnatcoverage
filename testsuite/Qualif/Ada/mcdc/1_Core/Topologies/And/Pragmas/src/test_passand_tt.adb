with Passand;

-- eval True only, all stmts covered

procedure Test_Passand_TT is
begin
   Passand (True, True);
end;

--# passand.ads
--  /eval/ l! eF-

--# passand.adb
--  /eval/ l! eF-
--  /stmt/ l+ 0
