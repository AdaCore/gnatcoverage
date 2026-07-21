with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Process (B1, B2, B3 : Boolean) is
   begin
      if B1 then
         Put_Line ("Message 1");
      elsif B2 then
         Put_Line ("Message 2");
      elsif B3 then
         Put_Line ("Message 3");
      else
         Put_Line ("Message 4");
      end if;
   end Process;
end Pkg;
