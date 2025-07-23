with Ada.Text_IO; use Ada.Text_IO;

procedure Main_C_Process is
   procedure Unit_C_Process;
   pragma Import (C, Unit_C_Process, "unit_c_process");
begin
   Put_Line ("Start main");
   Unit_C_Process;
   Put_Line ("End main");
   pragma Annotate (Xcov, Dump_Buffers);
end Main_C_Process;
