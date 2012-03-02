with Passor, Silent_Last_Chance;

-- eval True and False, indep(B) only

procedure Test_Passor_B is
begin
   Passor (False, True);
   Passor (False, False);
exception
    when others => null;
end;

--# passor.ads
--  /eval/ l! c!:"A "

--# passor.adb
--  /eval/ l! c!:"A "
--  /stmt/ l+ 0
