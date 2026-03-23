with Support; use Support;

with Pkg.Child;
with Pkg_Child;

procedure Test_Collision is
begin
   Assert (Pkg.Child.Foo (3) = Pkg_Child.Bar (3));
end Test_Collision;

--# pkg-child.adb pkg_child.adb
--
-- /stmt/ l+ ## 0
