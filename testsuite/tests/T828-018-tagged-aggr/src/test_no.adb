with Pkg;

procedure Test_No is
begin
   Pkg.Foo;
end Test_No;

--# pkg.ads
-- /foo/ l. ## 0
--
--# pkg.adb
-- /create/ l- ## s-
-- /foo/    l+ ## 0
