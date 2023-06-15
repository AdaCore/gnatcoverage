with Ada.Text_IO; use Ada.Text_IO;
with Helpers;

separate (Pkg)
procedure Test_Driver is

begin

   if Is_Even (Helpers.Id (Helpers.Id (1))) then
      Put_Line ("Error!");
   end if;

end Test_Driver;
