with Pkg;

procedure Test_Full is
begin
   Pkg.Process (ASCII.NUL);
   Pkg.Process ('Y');
   Pkg.Process ('m');
   Pkg.Process ('5');
   Pkg.Process ('+');
end Test_Full;

--# pkg.adb
--
-- /case/         l+ ## 0
-- /when_nul/     l+ ## 0
-- /when_upper/   l+ ## 0
-- /when_lower/   l# ## x0:"J1"
-- /when_lower_s/ l= ## 0
-- /when_digit/   l+ ## 0
-- /when_other/   l# ## x0:"J2"
-- /when_other_s/ l= ## 0
