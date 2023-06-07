package body Dotted is

   package body Simple_Decl is
      procedure Foo is
      begin
         null;
      end Foo;
   end Simple_Decl;

   procedure Bar is
   begin
      raise Program_Error;
   end Bar;

end Dotted;