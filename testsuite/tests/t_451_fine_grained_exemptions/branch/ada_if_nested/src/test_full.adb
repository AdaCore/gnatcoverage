with Pkg;

procedure Test_Full is
begin
   Pkg.Process (False, False);
   Pkg.Process (True, False);
   Pkg.Process (True, True);
end Test_Full;

--# pkg.adb
--
-- /cond_1/          l+ ## 0
-- /cond_2/          l# ## 0
-- /exempt/          l# ## x0:"J"
-- /exempt_put_line/ l= ## 0
