with Support, Values, Silent_Last_Chance; use Support, Values;

-- Set both I1 and I2 only. Precond True only.

procedure Test_Values_TT is
   I1, I2 : Int;
begin
   Set (I1, 5);	
   Set (I2, 3);	
   Assert (not Same (I1, I2));
end;

--# values.adb
-- /pre/   l! dF-
-- /same/  l+ 0
-- /set/   l+ 0
-- /value/ l+ 0
