with Ada.Text_IO;  use Ada.Text_IO;

package body Lib is

   procedure Foo is
   begin
      Put_Line ("foo");
   end;

begin
   Put_Line ("elaboration");
end;
