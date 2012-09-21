with Passor, Silent_Last_Chance;

-- eval False only, body stmt uncovered

procedure Test_Passor_FF is
begin
   Passor (False, False);
exception
    when others => null;
end;

--# passor.ads
--  /eval/ l! ## eT-

--# passor.adb
--  /eval/ l! ## eT-
--  /stmt/ l- ## s-
