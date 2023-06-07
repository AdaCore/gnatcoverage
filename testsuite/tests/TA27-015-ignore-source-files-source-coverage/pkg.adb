with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Say_Hello is
   begin
      if False then
         Put_Line ("Hello, world!");
      else
      Put_Line ("Not Hello World!");
      end if;
   end Say_Hello;
   procedure Say_Goodbye is separate;
end Pkg;
