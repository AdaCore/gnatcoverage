with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Run is
      Word : constant String := "world";
   begin
      if $X = "world" then
         Put_Line ("Hello, " & $X & "!");
      end if;
   end Run;

end Pkg;
