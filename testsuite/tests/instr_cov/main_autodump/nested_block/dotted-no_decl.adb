procedure Dotted.No_Decl is
begin
   Block : declare
      procedure Foo is
      begin
         null;
      end Foo;
   begin
      Dotted.No_Decl.Block.Foo;
      No_Decl.Block.Foo;
   end Block;
end Dotted.No_Decl;
