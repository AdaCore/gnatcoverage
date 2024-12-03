with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_False is
begin
   Put_Line (Foo (Cow));
end Test_False;

--# pkg.adb
-- /if/              l+ ## 0
-- /case/            l! ## dT-
-- /when_dog_cat/    l! ## g-
-- /when_cow/        l! ## 0
-- /return_pet/      l- ## s-
-- /return_cattle/   l+ ## 0
