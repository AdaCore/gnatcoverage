with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print (I : Integer) is
   begin
      Put_Line (Integer'Image (I));  -- # stmt
   end Print;
end Pkg;
