with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_Full is
begin
   Put_Line (Foo (Dog));
   Put_Line (Foo (Cow));
end Test_Full;

--# pkg.adb
-- /if/        l+ ## 0
-- /case/      l+ ## 0
-- /when/      l+ ## 0
-- /return/    l+ ## 0
