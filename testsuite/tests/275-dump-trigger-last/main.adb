with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   Put_Line ("This is a first statement");
   pragma Annotate (Xcov, Dump_Buffers);
   Put_Line ("This is a second statement");
end Main;
