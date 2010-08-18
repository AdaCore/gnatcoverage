with Counters, Support; use Counters, Support;

-- Invoke Dec case only. Verify that the Inc case is reported uncovered.

procedure Test_Counters_Dec is
   X : Integer := 0;
begin
   Step_By (2, X, Dec);
   Assert (X = -2);
end;

--# counters.adb
--  /case/  l+ 0
--  /opInc/ l- s-
--  /opDec/ l+ 0
