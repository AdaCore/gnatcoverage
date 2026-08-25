with Pkg;

procedure Main is
   X : constant Integer := Pkg.Ext_Return (1);
   pragma Unreferenced (X);
begin
   null;
end Main;
