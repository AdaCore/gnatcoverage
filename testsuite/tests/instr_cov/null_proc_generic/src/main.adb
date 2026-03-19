with Pkg;

procedure Main is
   procedure P is new Pkg.P (Integer);
begin
   P (0);
end Main;
