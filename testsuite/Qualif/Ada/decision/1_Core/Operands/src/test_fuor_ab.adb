with FUOR_Helper;

procedure Test_FUOR_AB is
begin
   FUOR_Helper.Eval_FF_F;
   FUOR_Helper.Eval_TX_T;
   FUOR_Helper.Eval_FT_T;
end;

--# fuor.adb
--  /eval0/  l+ ## 0
--  /eval1/  l+ ## 0c
--  /true/   l+ ## 0
--  /false/  l+ ## 0
