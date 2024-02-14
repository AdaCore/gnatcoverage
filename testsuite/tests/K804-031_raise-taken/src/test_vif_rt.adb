with Args, Vif; use Args, Vif;

procedure Test_Vif_RT is
begin
   Check_Ge0 (Num'Last);
   Check_Ge0 (1);
end;

--# vif.adb
--  /eval/    l! ## dF-
--  /true/    l+ ## 0
--  /handler/ l+ ## 0
