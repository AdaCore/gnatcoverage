with Monitor;
procedure Test_Ifcst_None is
   X : Integer := 0;
   pragma Volatile (X);
begin
   X := X + 1;
end;

--# monitor.adb
--  /called/ l- ## s-
