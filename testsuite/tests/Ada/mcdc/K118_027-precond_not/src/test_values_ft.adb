with Support, Values; use Support, Values;

procedure Test_Values_FT is
   I1 : Int;
begin
   --  Cheat to force the first precond operand False
   I1 := (Set => True, Val => 5);
   Set (I1, 2, Reset_OK => True);
end;

--# values.adb
-- /pre/   l! ## dF-
-- /set/   l+ ## 0

