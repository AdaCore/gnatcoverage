with Support, Values; use Support, Values;

procedure Test_F is
begin
   Assert (not Even_Odd (2, 8));
   Assert (not Even_Odd (1, 7));
end;

--# values.ads
--  /eval/ l! ## eT-
