pragma Ada_2012;
with Pkg;

procedure Main
is
   Obj : Pkg.T_Child;
   B   : Boolean := Obj.T_Prim (True, False);
begin
   null;
end Main;
