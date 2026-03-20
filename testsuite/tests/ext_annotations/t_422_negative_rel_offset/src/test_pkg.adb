with Pkg;

procedure Test_Pkg is
begin
   Pkg.Check_Ok;
end Test_Pkg;


--# pkg.adb
--
-- /ok_st/ l+ ## 0
-- /ex/    l* ## x+
-- /ex_dc/ l= ## XoT-
-- /ex_st/ l= ## Xs-
