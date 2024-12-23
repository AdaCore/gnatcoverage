pragma Ada_2012;
with Ada.Text_IO;
with Pkg1;
with Pkg2;

package body Greet is
   procedure Say_Hi is
   begin
      Ada.Text_IO.Put_Line
         ("Pkg.Val: " &
           (if Pkg1.Val or else Pkg2.Val then "True" else "False"));
   end Say_Hi;
end Greet;
