with Monitor;
procedure Test_Ifcst_Call is
   X : Integer := 0;
   pragma Volatile (X);
begin
   X := X + 1;
   Monitor.Op;
end;

--# monitor.adb
--  /called/    l+ ## 0
--
--%opts: --trace-mode=bin
--  /cond-stmt/ l. ## 0
--
--%opts: --trace-mode=src
--  /cond-stmt/ l- ## s-
