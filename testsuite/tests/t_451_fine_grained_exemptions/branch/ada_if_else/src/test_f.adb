with Pkg;

procedure Test_F is
begin
   Pkg.Process (False);
end Test_F;

--# pkg.adb
--
-- /condition/       l! ## dT-
-- /put_line_1/      l- ## s-
-- /exempt/          l# ## x0:"J"
-- /exempt_put_line/ l= ## 0
