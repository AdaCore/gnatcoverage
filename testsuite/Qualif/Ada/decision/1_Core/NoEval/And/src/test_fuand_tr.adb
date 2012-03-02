with Silent_Last_Chance, Support, FUAND_Helper; use Support;

procedure Test_FUAND_TR is
begin
   FUAND_Helper.Eval_TR;
exception
    when others => null;
end;

--# fuand.adb
--  /eval/  l! d-
--  /true/  l- s-
--  /false/ l- s-
