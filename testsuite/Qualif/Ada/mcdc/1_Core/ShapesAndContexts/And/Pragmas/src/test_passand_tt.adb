with Passand;

-- eval True only, all stmts covered

procedure Test_Passand_TT is
begin
   Passand (True, True);
end;

--# passand.ads
--  /eval/ l! dF-

--# passand.adb
--  /eval/ l! dF-
--  /stmt/ l+ 0
