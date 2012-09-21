with Support, Counters; use Support;

-- Call the decrementer only and verify that the incrementer code
-- is reported uncovered.

procedure Test_Counters_Dec is
begin
   Counters.Dec_X;
   Assert (Counters.X = -1);
end;

--# counters.adb
-- /IncX/ l- ## s-
