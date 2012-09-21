with Support, Counters; use Support;

-- Call none of the code and verify that everything is reported uncovered.

procedure Test_Counters_0 is
begin
   Assert (True);
end;

--# counters.adb
-- /./ l- ## s-
