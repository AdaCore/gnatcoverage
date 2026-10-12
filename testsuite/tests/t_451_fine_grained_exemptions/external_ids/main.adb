pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   function Identity (Self : Boolean) return Boolean is (Self);
   F : constant Boolean := Identity (False);
   T : constant Boolean := Identity (True);
begin
   if Identity (F and then T) or else Identity (T or else F) then
      Put_Line ("Hello world");
   end if;
end Main;
