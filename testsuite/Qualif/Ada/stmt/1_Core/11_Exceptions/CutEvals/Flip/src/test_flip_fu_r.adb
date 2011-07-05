with Silent_Last_Chance, Support, Flip_Helper; use Support, Flip_Helper;

procedure Test_Flip_FU_R is
begin
   Flip_Helper.Eval_T;
   Flip_Helper.Eval_F;
   Flip_Helper.Eval_R;
end;

--# flip.adb
--  /eval/  l+ 0
--  /false/  l+ 0
--  /true/ l+ 0
