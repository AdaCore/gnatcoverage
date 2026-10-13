with Ada.Text_IO; use Ada.Text_IO;

with Func_Helpers;

procedure Main is
   use Func_Helpers;
   F : constant Boolean := Identity (False);
   T : constant Boolean := Identity (True);
begin
   if Identity (F and then T) or else Identity (T or else F) then
      Put_Line ("Hello world");
   end if;
end Main;
