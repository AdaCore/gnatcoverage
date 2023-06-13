with Support, Values; use Support, Values;
with Silent_Last_Chance;

procedure Test_F is
   X : Integer;
begin
   X := Plus (5, -8);
exception
   when others => null;
end;

--# values.ads
--  /eval/ l! ## dT-
