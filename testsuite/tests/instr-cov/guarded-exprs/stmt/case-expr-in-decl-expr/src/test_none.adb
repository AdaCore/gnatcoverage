pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Life;        use Life;

procedure Test_None is
begin
   null;
end Test_None;

--# life.adb
-- /return/    l- ## s-
-- /decl/      l- ## 0
-- /sound/     l- ## s-
-- /case/      l- ## 0
-- /dog/       l- ## g-
-- /cat/       l- ## g-
-- /cow/       l- ## g-
-- /begin/     l- ## 0
-- /cmp/       l- ## 0
