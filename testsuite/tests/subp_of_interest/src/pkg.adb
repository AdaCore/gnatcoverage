with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Foo (I : Integer) is
   begin
      Put_Line ("Hello from Foo!");
   end Foo;

   procedure Bar is
   begin
      Put_Line ("Hello from Bar!");
   end Bar;

end Pkg;
