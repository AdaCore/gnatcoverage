with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Foo is
   begin
      Put_Line ("Hello from main-body");
   end Foo;

   procedure Bar is separate;
end Pkg;
