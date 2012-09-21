with Passor;

-- eval True only (from B)

procedure Test_Passor_FT is
begin
   Passor (False, True);
end;

--# passor.ads
--  /eval/ l! ## eF-

--# passor.adb
--  /eval/ l! ## eF-
--  /stmt/ l+ ## 0
