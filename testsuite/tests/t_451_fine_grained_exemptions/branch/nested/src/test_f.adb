with Pkg;

procedure Test_F is
begin
   Pkg.Process (False, False);
end Test_F;

--# pkg.adb
--
-- /cond1/            l* ## XoT-
-- /ex1/              l* ## x+:"J1"
-- /ex1_put_line/     l= ## Xs-
-- /ex1_cond2/        l= ## Xs-
-- /ex1_ex2/          l= ## 0
-- /ex1_ex2_put_line/ l= ## Xs-
-- /put_line/         l+ ## 0
