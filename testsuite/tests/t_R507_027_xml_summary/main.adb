with Data;
with Pkg;

procedure Main is
   X : Boolean := Pkg.F1 (Data.One);
   Y : Boolean := Pkg.F2 (Data.One);
begin
   null;
end Main;
