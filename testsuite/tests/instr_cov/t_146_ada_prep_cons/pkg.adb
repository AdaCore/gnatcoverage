with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Say_Hello is
   begin
#if SAY_HI then
      Put_Line ("Hi world!");
#else
      Put_Line ("Hello world!");
#end if;
   end Say_Hello;
end Pkg;
