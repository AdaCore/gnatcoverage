with Silent_Last_Chance, Support, FUOR_Helper; use Support;

procedure Test_FUOR_RX is
begin
   FUOR_Helper.Eval_RX;
exception
    when others => null;
end;

--# fuor.adb
--  /eval/  l+ ## 0
--  /true/  l- ## s-
--  /false/ l- ## s-
