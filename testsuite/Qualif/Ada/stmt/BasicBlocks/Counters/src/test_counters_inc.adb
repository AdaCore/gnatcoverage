with Support, Counters; use Support;

-- Call the incrementer only and verify that the decrementer code
-- is reported uncovered.

procedure Test_Counters_Inc is
begin
   Counters.Inc_X;
   Assert (Counters.X = 1);
end;

--# counters.adb
-- /DecX/ l- s-
