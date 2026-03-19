with Pkg;

procedure Test is
   package Pkg_Inst is new Pkg (2);
begin
   Pkg_Inst.Foo;
end Test;
