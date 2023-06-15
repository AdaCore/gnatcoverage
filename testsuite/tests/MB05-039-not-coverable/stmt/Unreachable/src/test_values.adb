with State, Support, Values; use State, Support, Values;

procedure Test_Values is
begin
   Assert (Abs_Of(5) = 5);
   Assert (Abs_Of(-2) = 2);
end;

--# values.adb
--  /stmt/ l+ ## 0
--  /xpos/ l+ ## 0
--  /neg/  l+ ## 0
--  /out/  l0 ## s0
