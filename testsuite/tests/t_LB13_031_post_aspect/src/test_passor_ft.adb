with Passor, Silent_Last_Chance;

-- evalcond True only (from B), all stmt covered

procedure Test_Passor_FT is
begin
   Passor (False, True);
exception
    when others => null;
end;

--# passor.ads
--  /eval/ l! ## dF-

--# passor.adb
--  /stmt/ l+ ## 0
