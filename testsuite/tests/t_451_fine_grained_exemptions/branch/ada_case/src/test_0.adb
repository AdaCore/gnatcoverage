with Pkg;

procedure Test_0 is
begin
   null;
end Test_0;

--# pkg.adb
--
-- /case/         l- ## s-
-- /when_nul/     l- ## s-
-- /when_upper/   l- ## s-
-- /when_lower/   l* ## x+:"J1"
-- /when_lower_s/ l= ## Xs-
-- /when_digit/   l- ## s-
-- /when_other/   l* ## x+:"J2"
-- /when_other_s/ l= ## Xs-
