with Support, Values, Silent_Last_Chance; use Support, Values;

-- Set I1 only. I2 not set - precond False only, silent raise

procedure Test_Values_TF is
   I1, I2 : Int;
begin
   Set (I1, 5);
   Assert (not Same (I1, I2));
end;

--# values.adb
-- /pre/   l! dT-
-- /set/   l+ 0
-- /same/  l- s-
-- /value/ l- s-
