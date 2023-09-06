with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   -------------------
   -- Print_Message --
   -------------------

   procedure Print_Message (Goodbye : Boolean) is
   begin
      if Goodbye then
         Put_Line ("Goodbye!");
      else
         Put_Line ("Hello!");
      end if;
   end Print_Message;

end Pkg;
