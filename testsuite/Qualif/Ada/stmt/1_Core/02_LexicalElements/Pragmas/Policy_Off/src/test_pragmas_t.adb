with Support, Pragmas; use Pragmas, Support;

--  Invoke the two subprograms, each featuring Assert and Debug
--  pragmas. Compilation-wise, the pragmas are inactive and the
--  Log_Debug subprogram is never called.

--  pragma Debug is perceived as an active "statement" by the
--  source instrumenter, though, and pragma Assert not.

procedure Test_Pragmas_T is
   R : Integer;
begin
   Sum (X => 2, Y => 3, UB => 10, R => R);
   Assert (R = 5);
   Dif (X => 5, Y => 1, LB => 1, R => R);
   Assert (R = 4);
   Assert (Debug_Events = 0);
end;

--# pragmas.adb
-- /eval/   l+ ## 0
-- /log/    l- ## s-

--%opts:--trace-mode=bin
-- /debug/  l. ## 0
-- /assert/ l. ## 0

--%opts:--trace-mode=src
-- /debug/  l+ ## 0
-- /assert/ l. ## 0
