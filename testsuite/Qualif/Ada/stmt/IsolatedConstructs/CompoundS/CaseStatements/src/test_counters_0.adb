with Counters, Support; use Counters, Support;

-- Don't invoke anything. Verify that everything is reported uncovered.

procedure Test_Counters_0 is
begin
   Assert (True);
end;

--# counters.adb
--  /case/  l- s-
--  /opInc/ l- s-
--  /opDec/ l- s-
