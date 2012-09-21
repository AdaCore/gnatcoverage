with Support, Pval, Silent_Last_Chance; use Support, Pval;

procedure Test_Pval_F is
begin
   Assert (F (False) = False);
exception
    when others => null;
end;

--# pval.ads
-- /eval/    l! ## o!

--# pval.adb
-- /eval/        l! ## o!
-- /returnTrue/  l- ## s-
-- /returnFalse/ l- ## s-
-- /returnVal/   l- ## s-
