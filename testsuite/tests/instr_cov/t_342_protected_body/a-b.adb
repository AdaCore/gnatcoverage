separate (A)

protected body B
is

   procedure Foo
   is
      Decl : Integer;
   begin
      if Decl = 1 then
         null;
      else
         null;
      end if;
   end Foo;

   procedure Bar
   is
   begin
      null;
   end Bar;

   procedure Baz
   is
   begin
      null;
   end Baz;

end B;
