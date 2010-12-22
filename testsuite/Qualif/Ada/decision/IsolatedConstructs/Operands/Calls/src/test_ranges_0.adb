with Ranges, Support; use Ranges, Support;

-- Exercise nothing. Expect all statements reported uncovered.

procedure Test_Ranges_0 is   
begin
   Assert (True);
end;

--# ranges.adb
--  /checkValid/    l- s-
--  /assignValid/   l- s-
--  /assignInvalid/ l- s-
--  /preValid/      l- s-
--  /retStmt/       l- s-
--  /retLine/       l- 0

