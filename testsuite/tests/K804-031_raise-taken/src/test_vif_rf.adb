with Args, Vif; use Args, Vif;

procedure Test_Vif_RF is
begin
   Check_Ge0 (Num'Last);
   Check_Ge0 (-1);
end;

--# vif.adb
--  /eval/    l! ## dT-
--  /true/    l- ## s-
--  /handler/ l+ ## 0

