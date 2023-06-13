with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   ---------
   -- Run --
   ---------

   procedure Run (A, B, C : Boolean) is
   begin
      if A then
         Put_Line ("A is true");
      end if;
      if B then
         Put_Line ("B is true");
      end if;
      if C then
         Put_Line ("C is true");
      end if;
   end Run;

end Pkg;
