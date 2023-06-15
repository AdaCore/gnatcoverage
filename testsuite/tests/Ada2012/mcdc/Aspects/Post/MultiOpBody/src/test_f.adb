with Support, Values; use Support, Values;
with Silent_Last_Chance;

procedure Test_F is
   A : Int := (Value => 5, Valid => True);
   B : Int := (Value => -7, Valid => True);
begin
   Filter (A, B);
exception
   when others => null;
end;

--# values.adb
--  /eval/ l! ## dT-
