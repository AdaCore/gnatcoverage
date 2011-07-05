with Silent_Last_Chance, Support, Val_Helper; use Support, Val_Helper;

procedure Test_Val_FU_R is
begin
   Val_Helper.Eval_T;
   Val_Helper.Eval_F;
   Val_Helper.Eval_R;
end;

--# val.adb
--  /eval/  l+ 0
--  /true/  l+ 0
--  /false/ l+ 0
