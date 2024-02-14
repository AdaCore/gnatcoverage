procedure Simple_No_Decl is
begin
   Block : declare
      procedure Foo is
      begin
         null;
      end Foo;
   begin
      Simple_No_Decl.Block.Foo;
   end Block;
end Simple_No_Decl;
