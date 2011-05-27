with Passor, Silent_Last_Chance;

-- evalcond true and false, all stmt covered

procedure Test_Passor_FU is
begin
   Passor (True, False);
   Passor (False, False);
end;

--# passor.ads
--  /eval/ l+ 0

--# passor.adb
--  /eval/ l+ 0
--  /stmt/ l+ 0
