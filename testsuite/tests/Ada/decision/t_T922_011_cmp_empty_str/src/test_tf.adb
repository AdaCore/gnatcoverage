with Ada.Text_IO; use Ada.Text_IO;

with Pkg;

procedure Test_TF is
begin
   Pkg.Dump (Pkg.A);
   Pkg.Dump (Pkg.C);
end Test_TF;

--# pkg.adb
-- /decl/ l+ ## 0
-- /cond/ l+ ## 0
-- /then/ l+ ## 0
