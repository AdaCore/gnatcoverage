with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   -------------
   -- Compute --
   -------------

   procedure Compute (A, B : Boolean) is
   begin
      if A and then B then
         Put_Line ("true");
      else
         Put_Line ("false");
      end if;
   end Compute;

end Pkg;
