with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Run is
   begin
#if $X then
         Invalid Statement
#else
         Put_Line ("Hello, world!");
#end if;

#if not $X then
         Put_Line ("Hello, world!");
#else
         Invalid Statement
#end if;
   end Run;

end Pkg;
