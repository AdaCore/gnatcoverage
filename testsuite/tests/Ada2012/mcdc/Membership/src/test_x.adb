with Support, Values; use Support, Values;

procedure Test_X is
begin
   Assert (Even_Odd (4, 7));
   Assert (not Even_Odd (1, 1));
end;

--# values.ads
--  /eval/ l! ## c!:"Y"
