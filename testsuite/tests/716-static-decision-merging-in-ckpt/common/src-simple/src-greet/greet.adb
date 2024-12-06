pragma Ada_2012;
with Ada.Text_IO;
with Pkg;

package body Greet is
   procedure Say_Hi is
   begin
      Ada.Text_IO.Put_Line
         ("Pkg.Val: " & (if Pkg.Val then "True" else "False"));
   end Say_Hi;
end Greet;
