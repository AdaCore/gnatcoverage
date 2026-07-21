with Pkg;

procedure Test_Lower is
begin
   Pkg.Process ('d');
end Test_Lower;

--# pkg.adb
--
-- /case/         l+ ## 0
-- /when_nul/     l- ## s-
-- /when_upper/   l- ## s-
-- /when_lower/   l# ## x0:"J1"
-- /when_lower_s/ l= ## 0
-- /when_digit/   l- ## s-
-- /when_other/   l* ## x+:"J2"
-- /when_other_s/ l= ## Xs-
