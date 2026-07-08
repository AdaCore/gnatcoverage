with Interfaces.C; use Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   function Ident (X : int) return int;
   pragma Import(Convention => C, Entity => Ident, External_Name => "ident");

   X   : int := 5;
   Res : int := Ident (X);
begin
   if X /= Res then
      Put_Line ("C really does weird things with their integers");
   end if;
end Main;
