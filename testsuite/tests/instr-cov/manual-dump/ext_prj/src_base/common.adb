with Ada.Text_IO; use Ada.Text_IO;

package body Common is

   procedure Say_Hello is
   begin
      Put_Line ("Hello from Base!");

      --  This is to check that this overridden source does not generate a
      --  trace, as it should not be compiled even after instrumentation.

      pragma Annotate (Xcov, Dump_Buffers);
   end Say_Hello;

end Common;
