with Passor, Silent_Last_Chance;

-- precond False only, all stmt covered

procedure Test_Passor_FF is
begin
   Passor (False, False);
end;

--# passor.ads
--  /eval/ l+ 0

--# passor.adb
--  /eval/ l+ 0
--  /stmt/ l+ 0
