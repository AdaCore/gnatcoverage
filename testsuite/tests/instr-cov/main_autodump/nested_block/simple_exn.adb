procedure Simple_Exn is
   procedure Foo is
   begin
      raise Program_Error;
   end Foo;

   procedure Bar is
   begin
      null;
   end Bar;
begin
   Bar;
   Simple_Exn.Foo;
exception
   when Program_Error =>
      Simple_Exn.Bar;
end Simple_Exn;