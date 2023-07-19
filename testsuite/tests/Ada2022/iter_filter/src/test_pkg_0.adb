with Support; use Support;

with Pkg; use Pkg;

procedure Test_Pkg_0 is
begin
   null;
end Test_Pkg_0;

--# pkg.adb
--
-- /filter/     l- ## 0
-- /loop_body/  l- ## s-
-- /other_stmt/ l- ## s-
-- /other_cont/ l- ## 0
