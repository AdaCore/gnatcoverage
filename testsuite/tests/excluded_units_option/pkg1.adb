with Ada.Text_IO;

package body Pkg1 is
   procedure Say_Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello from Pkg1");
   end Say_Hello;
end Pkg1;
