pragma Ada_2012;
with Ada.Text_IO;
with Pkg;

package body Greet is
   procedure Say_Hi is
   begin
      if Pkg.Val then
         Ada.Text_IO.Put_Line ("Pkg.Val: True");
      end if;
   end Say_Hi;
end Greet;
