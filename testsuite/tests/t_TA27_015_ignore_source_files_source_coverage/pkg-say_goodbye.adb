with Ada.Text_IO; use Ada.Text_IO;

separate (Pkg)
procedure Say_Goodbye is
begin
   if True then
      Put_Line ("Goodbye!");
   else
      Put_Line ("But not really!");
   end if;
end Say_Goodbye;
