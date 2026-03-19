with Ada.Text_IO; use Ada.Text_IO;

with Pkg;

procedure Test_No is
begin
   Put_Line (Pkg.Enum_Images (Pkg.A).all);
end Test_No;

--# pkg.adb
-- /decl/ l- ## s-
-- /cond/ l- ## s-
-- /then/ l- ## s-
