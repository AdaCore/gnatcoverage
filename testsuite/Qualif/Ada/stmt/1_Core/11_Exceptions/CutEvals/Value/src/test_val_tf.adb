with Support, Val_Helper; use Support, Val_Helper;

procedure Test_Val_TF is
begin
   Val_Helper.Eval_F;
   Val_Helper.Eval_T;
end;

--# val.adb
--  /eval/  l+ 0
--  /true/  l+ 0
--  /false/ l+ 0
