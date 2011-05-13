with Silent_Last_Chance, Support, Flip_Helper; use Support, Flip_Helper;

procedure Test_Flip_T_R is
   V : Boolean;
begin
   Flip_Helper.Eval_T;
   Flip_Helper.Eval_R;
end;

--# flip.adb
--  /eval/  l! d!
--  /false/  l+ 0
--  /true/ l- s-
