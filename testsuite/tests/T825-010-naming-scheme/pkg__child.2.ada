with Ada.Text_IO; use Ada.Text_IO;

package body Pkg.Child is
   procedure Proc (I : Int) is
   begin
      Put_Line (Int'Image (I));
   end Proc;
end Pkg.Child;
