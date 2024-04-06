with Support, Pragmas; use Pragmas, Support;

--  Don't call into the functional code. Expect s- only on active lines
--  with real statements, not on the inactive pragma ones.

procedure Test_Pragmas_0 is
begin
   Assert (Pragmas.Debug_Events = 0);
end;

--# pragmas.adb
-- /eval/   l- ## s-
-- /log/    l- ## s-

--%opts:--trace-mode=bin
-- /debug/  l. ## 0
-- /assert/ l. ## 0

--%opts:--trace-mode=src
-- /debug/  l- ## s-
-- /assert/ l. ## 0
