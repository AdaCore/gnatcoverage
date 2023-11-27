with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Foo (I : Integer) is
   begin
      Put_Line ("Hello from Foo!");
   end Foo;

   procedure Bar is
      procedure Nested_Bar_1 is null;
      Dummy_Decl : Boolean;
      procedure Nested_Bar_2 is null;
   begin
      Nested_Bar_1;
      Nested_Bar_2;
      Put_Line ("Hello from Bar!");
   end Bar;

end Pkg;
