with Passand, Silent_Last_Chance;

-- eval False only (from B), body stmt uncovered

procedure Test_Passand_TF is
begin
   Passand (True, False);
exception
    when others => null;
end;

--# passand.ads
--  /eval/ l! ## dT-

--# passand.adb
--  /stmt/ l- ## s-
