with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Process (B1, B2 : Boolean) is
   begin
      if B1 then                                          -- # cond1
         pragma Annotate (Xcov, Exempt_Branch, "J1");     -- # ex1
         Put_Line ("Message 1");                          -- # ex1_put_line
         if B2 then                                       -- # ex1_cond2
            pragma Annotate (Xcov, Exempt_Branch, "J2");  -- # ex1_ex2
            Put_Line ("Message 2");                       -- # ex1_ex2_put_line
         end if;                                          -- # ex1
      else
         Put_Line ("No message");                         -- # put_line
      end if;
   end Process;
end Pkg;
