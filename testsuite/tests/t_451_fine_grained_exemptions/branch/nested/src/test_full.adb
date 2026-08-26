with Pkg;

procedure Test_Full is
begin
   Pkg.Process (False, False);
   Pkg.Process (True, False);
   Pkg.Process (True, True);
end Test_Full;

--# pkg.adb
--
-- /cond1/            l# ## 0
-- /ex1/              l# ## x0:"J1"
-- /ex1_put_line/     l= ## 0
-- /ex1_cond2/        l= ## 0
-- /ex1_ex2/          l= ## 0
-- /ex1_ex2_put_line/ l= ## 0
-- /put_line/         l+ ## 0
