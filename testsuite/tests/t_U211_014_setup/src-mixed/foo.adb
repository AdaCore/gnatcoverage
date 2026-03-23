pragma Ada_2012;

with Pkg;

procedure Foo is

   procedure Bar (Put_Char : Pkg.Put_Char_Access);
   pragma Import (C, Bar, "bar");

begin
   Bar (Pkg.Put_Char'Access);
end;
