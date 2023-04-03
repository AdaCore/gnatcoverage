with Silent_Last_Chance, Support, FUOR_Helper; use Support;

procedure Test_FUOR_T_FR is
begin
   FUOR_Helper.Eval_FT_T;
   FUOR_Helper.Eval_TX_T;
   FUOR_Helper.Eval_FR;
exception
    when others => null;
end;

--# fuor.adb
--  /eval/  l+ ## 0
--  /true/  l+ ## 0
--  /false/ l- ## s-
