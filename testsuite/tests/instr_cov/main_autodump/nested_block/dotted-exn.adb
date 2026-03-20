procedure Dotted.Exn is
   procedure Foo is
   begin
      raise Program_Error;
   end Foo;

   procedure Bar is
   begin
      null;
   end Bar;
begin
   Dotted.Exn.Bar;
   Exn.Foo;
exception
   when Program_Error =>
      Exn.Bar;
end Dotted.Exn;
