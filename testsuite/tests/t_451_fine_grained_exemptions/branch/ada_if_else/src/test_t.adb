with Pkg;

procedure Test_T is
begin
   Pkg.Process (True);
end Test_T;

--# pkg.adb
--
-- /condition/       l* ## XoF-
-- /put_line_1/      l+ ## 0
-- /exempt/          l* ## x+:"J"
-- /exempt_put_line/ l= ## Xs-
