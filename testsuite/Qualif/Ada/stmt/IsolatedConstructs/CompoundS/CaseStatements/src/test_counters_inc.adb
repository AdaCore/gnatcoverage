with Counters, Support; use Counters, Support;

-- Invoke Inc case only. Verify that the Dec case is reported uncovered.

procedure Test_Counters_Inc is
   X : Integer := 0;
begin
   Step_By (2, X, Inc);
   Assert (X = 2);
end;

--# counters.adb
--  /case/  l+ 0
--  /opInc/ l+ 0
--  /opDec/ l- s-
