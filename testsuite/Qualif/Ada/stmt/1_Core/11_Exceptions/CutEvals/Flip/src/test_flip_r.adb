with Silent_Last_Chance, Support, Flip_Helper; use Support, Flip_Helper;

procedure Test_Flip_R is
begin
   Flip_Helper.Eval_R;
exception
    when others => null;
end;

--# flip.adb
--  /eval/  l+ 0
--  /false/  l- s-
--  /true/ l- s-
