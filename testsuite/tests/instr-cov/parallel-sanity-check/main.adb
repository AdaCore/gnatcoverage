with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   function Identity (I : Integer) return Integer;
   pragma Import (C, Identity, "identity");
begin
   Put_Line (Integer'Image (Identity (1)));
end Main;
