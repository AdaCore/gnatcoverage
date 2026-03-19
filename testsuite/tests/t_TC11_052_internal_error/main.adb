pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type T is null record;
   procedure Do_Nothing is null;
begin
   Put_Line ("Hello World!");
   Do_Nothing;
end Main;
