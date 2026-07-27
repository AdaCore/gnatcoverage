with Pkg;

procedure Test_NUL is
begin
   Pkg.Process (ASCII.NUL);
end Test_NUL;

--# pkg.adb
--
-- /case/         l+ ## 0
-- /when_nul/     l+ ## 0
-- /when_upper/   l- ## s-
-- /when_lower/   l* ## x+:"J1"
-- /when_lower_s/ l= ## Xs-
-- /when_digit/   l- ## s-
-- /when_other/   l* ## x+:"J2"
-- /when_other_s/ l= ## Xs-
