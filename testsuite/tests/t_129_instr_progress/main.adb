with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure C_Func;
   pragma Import (C, C_Func, "c_func");
begin
   Put_Line ("Ada:main");
   C_Func;
end Main;
