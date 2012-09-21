with Support, Silent_Last_Chance, Pval; use Support, Pval;

procedure Test_Pval_T is
begin
   Assert (F (True) = False);
exception
    when others => null;
end;

--# pval.ads
-- /eval/    l! ## o!

--# pval.adb
-- /eval/    l! ## o!
-- /returnTrue/  l- ## s-
-- /returnFalse/ l- ## s-
-- /returnVal/   l- ## s-
