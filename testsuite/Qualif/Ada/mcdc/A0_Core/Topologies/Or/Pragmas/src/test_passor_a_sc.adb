with Passor, Silent_Last_Chance;

-- eval True and False, indep(A) despite change on B, shortcircuited

procedure Test_Passor_A_SC is
begin
   Passor (True, True);
   Passor (False, False);
exception
    when others => null;
end;

--# passor.ads
--  /eval/ l! ## c!:"B"

--# passor.adb
--  /eval/ l! ## c!:"B"
--  /stmt/ l+ ## 0
