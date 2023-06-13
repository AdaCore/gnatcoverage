with Support, Values; use Support, Values;

procedure Test_Y is
begin
   Assert (Even_Odd (4, 7));
   Assert (not Even_Odd (2, 8));
end;

--# values.ads
--  /eval/ l! ## c!:"X"
