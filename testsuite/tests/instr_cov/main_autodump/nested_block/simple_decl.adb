with Dotted;

procedure Simple_Decl is

   procedure Foo;
   procedure Bar;

   procedure Foo is
   begin
      null;
   end Foo;

   procedure Bar is
   begin
      Simple_Decl.Foo;
      Dotted.Simple_Decl.Foo;
   end Bar;
begin
   Simple_Decl.Bar;
   Dotted.Simple_Decl.Foo;
end Simple_Decl;
