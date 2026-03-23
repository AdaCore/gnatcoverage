with Pkg;
with Pkh;

with Support;

procedure Main is
begin
   Support.Assert (Pkg.Foo = 1);
   Support.Assert (Pkh.Bar = 2);
end Main;
