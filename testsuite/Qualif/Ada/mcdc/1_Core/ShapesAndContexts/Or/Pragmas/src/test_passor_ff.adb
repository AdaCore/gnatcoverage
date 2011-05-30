with Passor, Silent_Last_Chance;

-- eval False only, body stmt uncovered

procedure Test_Passor_FF is
begin
   Passor (False, False);
end;

--# passor.ads
--  /eval/ l! dT-

--# passor.adb
--  /eval/ l! dT-
--  /stmt/ l- s-
