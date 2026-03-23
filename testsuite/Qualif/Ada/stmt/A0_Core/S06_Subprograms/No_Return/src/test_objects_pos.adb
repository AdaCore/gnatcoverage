with Support; use Support;
with Silent_Last_Chance;
with Objects; use Objects;

procedure Test_Objects_Pos is
begin
   Proxy_Characterize (5);
   Assert (N_Positives = 1);
exception
   when others => null;
end;

--# objects.adb
--  /test/ l+ ## 0
--  /pos/  l+ ## 0
