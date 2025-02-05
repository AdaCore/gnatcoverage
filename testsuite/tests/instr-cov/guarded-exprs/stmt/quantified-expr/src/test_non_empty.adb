with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_Non_Empty is
    result : Boolean := Foo (1, 5);
begin
    null;
end Test_Non_Empty;

--# pkg.adb
--  /expr/  l+ ## 0
