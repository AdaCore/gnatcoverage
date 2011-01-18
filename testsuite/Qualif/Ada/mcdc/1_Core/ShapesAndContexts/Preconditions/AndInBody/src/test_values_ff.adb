with Support, Values, Silent_Last_Chance; use Support, Values;

-- No Set - precond False only, silent raise.

procedure Test_Values_FF is
   I1, I2 : Int;
begin
   Assert (not Same (I1, I2));
end;

--# values.adb
-- /pre/   l! dT-
-- /same/  l- s-
-- /set/   l- s-
-- /value/ l- s-
