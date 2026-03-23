with Support, Pval; use Support, Pval;

procedure Test_Pval_F is
begin
   Assert (F (False) = True);
end Test_Pval_F;

--# pval.adb
-- /eval/  l! ## dF-
-- /true/  l+ ## 0
-- /false/ l+ ## 0
-- /val/   l+ ## 0

-- Decision coverage with bin traces is imprecise on simple expressions

-- %opts: --trace-mode=bin
--
-- =/eval/ l! ## d!
