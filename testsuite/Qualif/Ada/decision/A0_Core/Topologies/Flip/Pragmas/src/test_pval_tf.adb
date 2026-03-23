with Support, Pval; use Support, Pval;

procedure Test_Pval_TF is
begin
   Assert (F (False) = True);
   Assert (F (True) = False);
end Test_Pval_TF;

--# pval.adb
-- /eval/      l+ ## 0
-- /set_true/  l+ ## 0
-- /set_false/ l+ ## 0
-- /ret_val/   l+ ## 0
