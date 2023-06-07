with Support, Values; use Support, Values;
with Silent_Last_Chance;

procedure Test_B is
   A : Int := (Value => 5, Valid => True);
   B : Int := (Value => -7, Valid => True);
   C : Int := (Value => 12, Valid => True);
begin
   Filter (A, C); -- T T
   Filter (A, B); -- T F
exception
   when others => null;
end;

--# values.adb
--  /eval/ l! ## c!:"A.Valid"
