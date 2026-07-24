with Pkg;

procedure Test_F is
begin
   Pkg.Process (False, False);
end Test_F;

--# pkg.adb
--
-- /cond_1/          l! ## dT-
-- /cond_2/          l- ## s-
-- /exempt/          l* ## x+:"J"
-- /exempt_put_line/ l= ## Xs-
