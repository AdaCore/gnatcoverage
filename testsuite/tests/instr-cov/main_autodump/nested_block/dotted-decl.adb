procedure Dotted.Decl is
   procedure Foo;
   procedure Bar;

   procedure Foo is
   begin
      null;
   end Foo;

   procedure Bar is
   begin
      Dotted.Decl.Foo;
      Decl.Foo;
   end Bar;

begin
   Dotted.Decl.Bar;
   Decl.Foo;
end Dotted.Decl;
