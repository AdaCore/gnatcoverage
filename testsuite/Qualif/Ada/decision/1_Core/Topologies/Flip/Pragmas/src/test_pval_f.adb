with Support, Pval; use Support, Pval;

procedure Test_Pval_F is
begin
   Assert (F (False) = True);
end;

--# pval.ads
-- /eval/    l! o!

--# pval.adb
-- /eval/        l! o!
-- /returnTrue/  l+ 0
-- /returnFalse/ l- s-
-- /returnVal/   l+ 0
