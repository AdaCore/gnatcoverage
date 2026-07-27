with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Process (B : Boolean) is
   begin
      if B then                                       -- # condition
         Put_Line ("Message 1");                      -- # put_line_1
      else
         pragma Annotate (Xcov, Exempt_Branch, "J");  -- # exempt
         Put_Line ("Message 2");                      -- # exempt_put_line
      end if;
   end Process;
end Pkg;
