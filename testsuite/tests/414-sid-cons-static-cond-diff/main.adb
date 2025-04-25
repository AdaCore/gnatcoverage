with Ada.Text_IO; use Ada.Text_IO;

with Pkg;
with Prelude;

procedure Main is
begin
   Prelude;
   if Pkg.B then
      Put_Line ("B is True");
   else
      Put_Line ("B is False");
   end if;
end Main;
