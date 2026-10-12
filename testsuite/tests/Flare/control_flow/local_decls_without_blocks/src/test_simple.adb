pragma Flare_0_1;
with Pkg;

procedure Test_Simple is
   Two_Squared : constant Integer := Pkg.Square (2);
begin
   null;
end Test_Simple;

--#  pkg.adb
--   /if/         l+ ## 0
--   /early/      l- ## s-
--   /local_decl/ l+ ## 0
--   /return/     l+ ## 0
