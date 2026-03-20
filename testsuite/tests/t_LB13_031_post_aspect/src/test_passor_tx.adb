with Passor;

-- evalcond True only (from A), all stmt covered

procedure Test_Passor_Tx is
begin
   for X in False .. True loop
      Passor (True, X);
   end loop;
end;

--# passor.ads
--  /eval/ l! ## dF-

--# passor.adb
--  /stmt/ l+ ## 0
