with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Finalize (Self : in out Ctrl_type) is
   begin
      Put_Line ("Got finalized");
   end Finalize;
end Pkg;
