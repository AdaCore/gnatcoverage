with Support, Values; use Support, Values;
with Silent_Last_Chance;

procedure Test_F is
   X : Integer;
begin
   X := Plus ((Value => 5, Valid => True), (Value => -7, Valid => False));
exception
   when others => null;
end;

--# values.ads
--  /eval/ l! ## dT-
