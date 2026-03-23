with Ada.Text_IO; use Ada.Text_IO;

procedure Main_C_Skipped is
   procedure Unit_C_Skipped;
   pragma Import (C, Unit_C_Skipped, "unit_c_skipped");
begin
   Put_Line ("Start main");
   Unit_C_Skipped;
   Put_Line ("End main");
   pragma Annotate (Xcov, Dump_Buffers);
end Main_C_Skipped;
