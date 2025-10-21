with Pkg;

procedure Main is
   Obj : constant Pkg.My_Tagged_Rec := (null record);
   A   : constant Boolean := Obj.Foo (1);
begin
   null;
end Main;
