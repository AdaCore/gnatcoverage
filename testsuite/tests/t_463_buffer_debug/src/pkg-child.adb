with Ada.Text_IO;

package body Pkg.Child is
   procedure Foo is
   begin
      Ada.Text_IO.Put_Line (Pkg_Name);
   end Foo;
end Pkg.Child;
