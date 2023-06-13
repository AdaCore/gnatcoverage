with Args, Vif; use Args, Vif;

procedure Test_Vif_R is
begin
   Check_Ge0 (Num'Last);
end;

--# vif.adb
--  /eval/    l! ## d-
--  /true/    l- ## s-
--  /handler/ l+ ## 0

