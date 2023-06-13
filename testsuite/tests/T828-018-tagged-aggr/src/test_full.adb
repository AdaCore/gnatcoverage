with Pkg;

procedure Test_Full is
   V : constant Pkg.T := Pkg.Create (1);
begin
   Pkg.Foo;
end Test_Full;

--# pkg.ads
-- /foo/ l. ## 0
--
--# pkg.adb
-- /create/ l+ ## 0
-- /foo/    l+ ## 0
