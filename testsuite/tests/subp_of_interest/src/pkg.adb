with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   ---------
   -- Foo --
   ---------

   procedure Foo (I : Integer) is
   begin
      Put_Line ("Hello from Foo!");
   end Foo;

   ---------
   -- Bar --
   ---------

   procedure Bar is
      procedure Nested_Bar_1 is null;
      procedure Nested_Bar_2;

      Dummy_Decl : Boolean;

      ------------------
      -- Nested_Bar_2 --
      ------------------

      procedure Nested_Bar_2 is
      begin
         Put_Line ("Hello from Nested_Bar_2!");
      end Nested_Bar_2;
   begin
      Nested_Bar_1;
      Nested_Bar_2;
      Put_Line ("Hello from Bar!");
   end Bar;

end Pkg;
