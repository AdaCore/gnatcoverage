with Monitor;
procedure Test_Ifcst_None is
   X : Integer := 0;
   pragma Volatile (X);
begin
   X := X + 1;
end;

--# monitor.adb
--  /called/    l- ## s-
--
--%opts: --trace-mode=bin
--  /cond-stmt/ l. ## 0
--
--%opts: --trace-mode=src
--  /cond-stmt/ l- ## s-
