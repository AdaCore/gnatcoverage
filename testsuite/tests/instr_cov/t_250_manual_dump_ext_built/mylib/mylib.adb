with Ada.Text_IO; use Ada.Text_IO;

package body Mylib is

   procedure Say_Hello is
   begin
      pragma Annotate (Xcov, Dump_Buffers);
      Put_Line ("Hello, world!");
   end Say_Hello;

end Mylib;
