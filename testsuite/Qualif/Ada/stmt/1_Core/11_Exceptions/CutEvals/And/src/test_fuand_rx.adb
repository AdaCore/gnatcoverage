with Silent_Last_Chance, Support, FUAND_Helper; use Support;

procedure Test_FUAND_RX is
begin
   FUAND_Helper.Eval_RX;
exception
    when others => null;
end;

--# fuand.adb
--  /eval/  l+ 0
--  /true/  l- s-
--  /false/ l- s-
