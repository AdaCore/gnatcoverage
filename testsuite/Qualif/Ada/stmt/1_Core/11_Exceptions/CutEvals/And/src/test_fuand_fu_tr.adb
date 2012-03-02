with Silent_Last_Chance, Support, FUAND_Helper; use Support;

procedure Test_FUAND_FU_TR is
begin
   FUAND_Helper.Eval_TT_T;
   FUAND_Helper.Eval_TF_F;

   FUAND_Helper.Eval_TR;
exception
    when others => null;
end;

--# fuand.adb
--  /eval/  l+ 0
--  /true/  l+ 0
--  /false/ l+ 0
