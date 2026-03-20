with Ada.Text_IO; use Ada.Text_IO;

with Dotted;

procedure Simple_Only_Exn is
begin
   Block : declare
      procedure Foo is
      begin
         Put_Line ("Foo was called");
      end Foo;
   begin
      Simple_Only_Exn.Block.Foo;
      Dotted.Bar;
   end Block;
exception
   when Program_Error =>
      Put_Line ("Program_Error");
   when Constraint_Error =>
      Put_Line ("Constraint_Error");
end Simple_Only_Exn;
