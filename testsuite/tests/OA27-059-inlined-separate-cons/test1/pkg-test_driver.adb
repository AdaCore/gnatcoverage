with Ada.Text_IO; use Ada.Text_IO;
with Helpers;

separate (Pkg)
procedure Test_Driver is
begin
   if not Is_Even (Helpers.Id (0)) then
      Put_Line ("Error!");
   end if;
end Test_Driver;
