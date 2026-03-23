with Pkg;

procedure Test_Full is
begin
   Pkg.Foo (0);
   Pkg.Bar (0);
   Pkg.Baz (0);
   Pkg.Blop (0);
end Test_Full;


--# pkg.adb
--
-- /st/   l+ ## 0
