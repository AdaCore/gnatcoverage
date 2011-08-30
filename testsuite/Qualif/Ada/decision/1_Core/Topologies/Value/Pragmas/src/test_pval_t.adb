with Support, Pval; use Support, Pval;

procedure Test_Pval_T is
begin
   Assert (F (True) = True);
end;

--# pval.ads
-- /eval/    l! o!

--# pval.adb
-- /eval/    l! o!
-- /returnTrue/  l+ 0
-- /returnFalse/ l- s-
-- /returnVal/   l+ 0
