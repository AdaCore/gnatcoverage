with Ada.Text_IO;

package body Pkg2 is
   procedure Say_Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello from Pkg2");
   end Say_Hello;
end Pkg2;
