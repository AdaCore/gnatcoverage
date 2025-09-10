with Support, Pval; use Support, Pval;

procedure Test_Pval_T is
begin
   Assert (F (True) = False);
end Test_Pval_T;

--# pval.adb
-- /eval/  l! ## dT-
-- /true/  l+ ## 0
-- /false/ l- ## s-
-- /val/   l+ ## 0

-- Decision coverage with bin traces is imprecise on simple expressions

-- %opts: --trace-mode=bin
--
-- =/eval/ l! ## d!
