with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_Empty is
    result : Boolean := Foo (1, 0);
begin
    null;
end Test_Empty;

--# pkg.adb
--  /expr/  l! ## g-
