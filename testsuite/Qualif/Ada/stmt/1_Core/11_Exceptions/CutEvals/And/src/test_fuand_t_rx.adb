with Silent_Last_Chance, Support, FUAND_Helper; use Support;

procedure Test_FUAND_T_RX is
begin
   FUAND_Helper.Eval_TT_T;
   FUAND_Helper.Eval_RX;
end;

--# fuand.adb
--  /eval/  l+ 0
--  /true/  l+ 0
--  /false/ l- s-
