with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Process (B1, B2 : Boolean) is
   begin
      if B1 then                                         -- # cond_1
         if B2 then                                      -- # cond_2
            pragma Annotate (Xcov, Exempt_Branch, "J");  -- # exempt
            Put_Line ("Message");                        -- # exempt_put_line
         end if;
      end if;
   end Process;
end Pkg;
