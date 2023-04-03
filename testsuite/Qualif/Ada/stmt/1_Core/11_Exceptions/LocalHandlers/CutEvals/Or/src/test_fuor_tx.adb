with Support, FUOR_Helper; use Support;

procedure Test_FUOR_TX is
begin
   Fuor_Helper.Eval_TX_T;
end;

--# fuor.adb
--  /eval/  l+ ## 0
--  /true/  l+ ## 0
--  /false/ l- ## s-
