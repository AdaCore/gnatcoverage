pragma Flare_0_1;
with Pkg;

procedure Test_Early_Return is
   Two_Squared : constant Integer := Pkg.Square (1);
begin
   null;
end Test_Early_Return;

--#  pkg.adb
--   /if/         l+ ## 0
--   /early/      l+ ## 0
--   /local_decl/ l- ## s-
--   /return/     l- ## s-
