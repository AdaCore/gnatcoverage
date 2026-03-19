with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Run is
   begin
#if X then
         Put_Line ("X");
#else
         Put_Line ("not X");
#end if;
   end Run;

end Pkg;
