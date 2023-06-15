with Ada.Text_IO; use Ada.Text_IO;

with Pkg;

procedure Test_T is
begin
   Pkg.Dump (Pkg.A);
end Test_T;

--# pkg.adb
-- /decl/ l+ ## 0
-- /cond/ l! ## dF-
-- /then/ l+ ## 0
