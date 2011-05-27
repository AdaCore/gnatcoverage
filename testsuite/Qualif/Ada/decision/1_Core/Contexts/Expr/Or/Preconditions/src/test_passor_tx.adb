with Passor;

-- precond True only (from A), all stmt covered

procedure Test_Passor_Tx is
begin
   for X in False .. True loop
      Passor (True, X);
   end loop;
end;

--# passor.ads
--  /eval/ l+ 0

--# passor.adb
--  /eval/ l+ 0
--  /stmt/ l+ 0
