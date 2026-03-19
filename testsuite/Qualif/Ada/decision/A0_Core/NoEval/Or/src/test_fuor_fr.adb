with Silent_Last_Chance, Support, FUOR_Helper; use Support;

procedure Test_FUOR_FR is
begin
   FUOR_Helper.Eval_FR;
exception
    when others => null;
end;

--# fuor.adb
--  /eval/  l! ## d-
--  /true/  l- ## s-
--  /false/ l- ## s-
