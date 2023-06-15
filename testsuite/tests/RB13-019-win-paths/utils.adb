with Ada.Text_IO; use Ada.Text_IO;

package body Utils is

   -----------
   -- Print --
   -----------

   procedure Print (Msg : chars_ptr) is
      Ada_Msg : constant String := Value (Msg);
   begin
      Put_Line (Ada_Msg);
   end Print;

end Utils;
