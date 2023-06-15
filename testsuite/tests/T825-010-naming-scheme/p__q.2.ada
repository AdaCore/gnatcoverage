with Ada.Text_IO; use Ada.Text_IO;

package body P.Q is
   procedure Proc (I : Int) is
   begin
      Put_Line (Int'Image (I));
   end Proc;
end P.Q;
