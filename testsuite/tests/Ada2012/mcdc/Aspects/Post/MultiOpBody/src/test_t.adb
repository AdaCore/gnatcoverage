with Support, Values; use Support, Values;

procedure Test_T is
   A : Int := (Value => 5, Valid => True);
   B : Int := (Value => 7, Valid => True);
begin
   Filter (A, B);
end;

--# values.adb
--  /eval/ l! ## dF-
