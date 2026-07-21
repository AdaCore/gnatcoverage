with Pkg;

procedure Main is
begin
   Pkg.Print_If (False, False, "some message");
   Pkg.Print_If (True, False, "some message");
   Pkg.Print_If (True, True, "some message");
end Main;
