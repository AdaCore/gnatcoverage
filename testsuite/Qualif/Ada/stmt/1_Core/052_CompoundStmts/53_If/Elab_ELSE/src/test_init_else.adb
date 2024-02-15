with Lib_If_Statements;
with Support; use Support;

procedure Test_Init_Else is
   X : Integer;
begin
   X := Lib_If_Statements.Probe (Slot => 1);
   Assert (X < 0);
end;

--# lib_if_statements.adb
--  /test_if/    l+ ## 0
--  /do_if/      l- ## s-
--  /test_elsif/ l+ ## 0
--  /do_elsif/   l- ## s-
--  /test_else/  l+ ## 0
--  /do_else/    l+ ## 0
