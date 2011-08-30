with Support, Silent_Last_Chance, Pval; use Support, Pval;

procedure Test_Pval_TF is
begin
   Assert (F (True) = True);
   Assert (F (False) = False);
end;

--# pval.adb
-- /eval/    l+ 0
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0
-- /returnVal/   l+ 0
