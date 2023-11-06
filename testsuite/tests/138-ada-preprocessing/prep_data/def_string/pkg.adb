with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Run is
      Word : constant String := $X;
   begin
      if Word = "world" then
         Put_Line ("Hello, " & Word & "!");
      end if;
   end Run;

end Pkg;
