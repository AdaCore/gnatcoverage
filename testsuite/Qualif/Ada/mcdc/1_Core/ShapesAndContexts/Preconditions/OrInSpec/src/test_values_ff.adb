with Support, Values, Silent_Last_Chance; use Support, Values;

procedure Test_Values_FF is
   I1 : Int;
begin
   --  Cheat to force the first precond operand False
   I1 := (Set => True, Val => 5);
   Set (I1, 2, Reset_OK => False);
end;

--# values.adb
-- /pre/   l! dT-
-- /set/   l- s-

