with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Process (B : Boolean) is
   begin
      if B then                                       -- # condition
         pragma Annotate (Xcov, Exempt_Branch, "J");  -- # exempt
         Put_Line ("Message 1");                      -- # exempt_put_line
      else
         Put_Line ("Message 2");                      -- # put_line_2
      end if;
   end Process;
end Pkg;
