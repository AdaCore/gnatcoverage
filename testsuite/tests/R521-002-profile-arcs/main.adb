with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   function Compute return Integer;
   pragma Import (C, Compute);
begin
   Put_Line ("Hello, world!" & Integer'Image (Compute));
end Main;
