with Ada.Text_IO; use Ada.Text_IO;

package body Harness is

   ------------------
   -- Assert_Equal --
   ------------------

   procedure Assert_Equal (Label : String; Expected, Actual : Integer) is
   begin
      if Expected = Actual then
         Put_Line ("OK   " & Label);
      else
         Put_Line ("FAIL " & Label);
      end if;
   end Assert_Equal;

end Harness;
