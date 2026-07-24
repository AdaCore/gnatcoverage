with Pkg;

procedure Test_Digit is
begin
   Pkg.Process ('9');
end Test_Digit;

--# pkg.adb
--
-- /case/         l+ ## 0
-- /when_nul/     l- ## s-
-- /when_upper/   l- ## s-
-- /when_lower/   l* ## x+:"J1"
-- /when_lower_s/ l= ## Xs-
-- /when_digit/   l+ ## 0
-- /when_other/   l* ## x+:"J2"
-- /when_other_s/ l= ## Xs-
