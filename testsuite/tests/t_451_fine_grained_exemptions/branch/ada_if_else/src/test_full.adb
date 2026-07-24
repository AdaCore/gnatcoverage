with Pkg;

procedure Test_Full is
begin
   Pkg.Process (False);
   Pkg.Process (True);
end Test_Full;

--# pkg.adb
--
-- /condition/       l# ## 0
-- /put_line_1/      l+ ## 0
-- /exempt/          l# ## x0:"J"
-- /exempt_put_line/ l= ## 0
