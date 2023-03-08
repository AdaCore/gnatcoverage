with Lib_If_Statements;
with Support; use Support;

procedure Test_Init_If is
   X : Integer;
begin
   X := Lib_If_Statements.Probe (Slot => 1);
   Assert (X > 0);
end;

--# lib_if_statements.adb
--  /test_if/    l+ ## 0
--  /do_if/      l+ ## 0
--  /test_elsif/ l- ## s-
--  /do_elsif/   l- ## s-
--  /test_else/  l- ## s-
--  /do_else/    l- ## s-

