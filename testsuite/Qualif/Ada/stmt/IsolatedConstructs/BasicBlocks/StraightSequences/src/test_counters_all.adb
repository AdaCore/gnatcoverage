with Support, Counters; use Counters, Support;

-- Call the incrementer and the decrementer, and verify that
-- nothing is reported uncovered.

procedure Test_Counters_All is
begin
   Inc_X; Dec_X;
   Assert (X = 0);
end;

--# counters.adb

