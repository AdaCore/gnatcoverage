with Passor;

-- eval True only (from A)

procedure Test_Passor_TF is
begin
   Passor (True, False);
end;

--# passor.ads
--  /eval/ l! ## eF-

--# passor.adb
--  /eval/ l! ## eF-
--  /stmt/ l+ ## 0
