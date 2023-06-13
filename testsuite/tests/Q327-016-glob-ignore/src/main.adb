with Pkg_A, Pkg_B, Pkg_C;
with Pkg_A.Test, Pkg_B.Test, Pkg_C.Test;

procedure Main is
begin
   Pkg_A.Main; Pkg_B.Main; Pkg_C.Main;
   Pkg_A.Test; Pkg_B.Test; Pkg_C.Test;
end Main;
