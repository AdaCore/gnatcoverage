with Ada.Text_IO; use Ada.Text_IO;

with Latin1;
with UTF8;

procedure Main is
begin
   if Latin1.F (True, True) and then UTF8.F (True, True) then
      Put_Line ("Hello world!");
   end if;
end Main;
