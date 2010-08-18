with Counters, Support; use Counters, Support;

-- Invoke both Dec and Inc. Verify that nothing is reported uncovered.

procedure Test_Counters_Full is
   X : Integer := 0;
begin
   Step_By (2, X, Inc);
   Step_By (1, X, Dec);
   Assert (X = 1);
end;

--# counters.adb
--  /case/  l+ 0
--  /opInc/ l+ 0
--  /opDec/ l+ 0
