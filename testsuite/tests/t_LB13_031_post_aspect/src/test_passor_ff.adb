with Passor, Silent_Last_Chance;

-- evalcond False only, body stmt covered

procedure Test_Passor_FF is
begin
   Passor (False, False);
exception
    when others => null;
end;

--# passor.ads
--  /eval/ l! ## dT-

--# passor.adb
--  /stmt/ l+ ## 0
