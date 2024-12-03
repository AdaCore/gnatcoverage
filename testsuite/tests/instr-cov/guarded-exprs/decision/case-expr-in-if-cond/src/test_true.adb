with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_True is
begin
   Put_Line (Foo (Cat));
end Test_True;

--# pkg.adb
-- /if/              l+ ## 0
-- /case/            l! ## dF-
-- /when_dog_cat/    l! ## 0
-- /when_cow/        l! ## g-
-- /return_pet/      l+ ## 0
-- /return_cattle/   l- ## s-
