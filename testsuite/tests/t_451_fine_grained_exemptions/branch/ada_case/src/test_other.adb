with Pkg;

procedure Test_Other is
begin
   Pkg.Process ('_');
end Test_Other;

--# pkg.adb
--
-- /case/         l+ ## 0
-- /when_nul/     l- ## s-
-- /when_upper/   l- ## s-
-- /when_lower/   l* ## x+:"J1"
-- /when_lower_s/ l= ## Xs-
-- /when_digit/   l- ## s-
-- /when_other/   l# ## x0:"J2"
-- /when_other_s/ l= ## 0
