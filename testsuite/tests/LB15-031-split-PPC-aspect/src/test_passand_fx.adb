with Passand, Silent_Last_Chance;

-- eval False only (from A), body stmt uncovered

procedure Test_Passand_FX is
begin
   Passand (False, True);
exception
    when others => null;
end;

--# passand.ads
--  /eval/ l! ## dT-

--# passand.adb
--  /stmt/ l- ## s-
