with Ada.Text_IO; use Ada.Text_IO;

with Pkg;

procedure Test_F is
begin
   Pkg.Dump (Pkg.C);
end Test_F;

--# pkg.adb
-- /decl/ l+ ## 0
-- /cond/ l! ## dT-
-- /then/ l- ## s-
