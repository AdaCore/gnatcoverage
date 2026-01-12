with Ada.Text_IO; use Ada.Text_IO;

with Params; use Params;

package body Pkg is

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      if A then
         Put_Line ("Hello world!");
      end if;
      Put_Line ("Done");
   end Run;

end Pkg;
