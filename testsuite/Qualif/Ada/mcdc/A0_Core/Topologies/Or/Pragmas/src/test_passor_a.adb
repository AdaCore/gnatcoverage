with Passor, Silent_Last_Chance;

-- eval True and False, indep(A) only

procedure Test_Passor_A is
begin
   Passor (True, False);
   Passor (False, False);
exception
    when others => null;
end;

--# passor.ads
--  /eval/ l! ## c!:"B"

--# passor.adb
--  /eval/ l! ## c!:"B"
--  /stmt/ l+ ## 0
