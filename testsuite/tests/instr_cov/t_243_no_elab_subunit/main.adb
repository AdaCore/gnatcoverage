with Ada.Text_IO; use Ada.Text_IO;
with Pkg;

procedure Main is
   use Pkg.Nested;
begin
   Set (1);
   if Get /= 1 then
      Put_Line ("ERROR");
      Pkg.Proc;
   end if;
end Main;
