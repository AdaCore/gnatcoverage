with Silent_Last_Chance, Support, Val_Helper; use Support, Val_Helper;

procedure Test_Val_F_R is
begin
   Val_Helper.Eval_F;
   Val_Helper.Eval_R;
exception
    when others => null;
end;

--# val.adb
--  /eval/  l+ 0
--  /true/  l- s-
--  /false/ l+ 0
