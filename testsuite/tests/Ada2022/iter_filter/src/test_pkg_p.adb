with Support; use Support;

with Pkg; use Pkg;

procedure Test_Pkg_P is
   Input_Arr : Arr_T := (1 => 1);
begin
   Assert (Non_Zero_Mult (Input_Arr) = 1);
end Test_Pkg_P;

--# pkg.adb
--
-- /filter/     l! ## dF-
-- /loop_body/  l+ ## 0
-- /other_stmt/ l+ ## 0
-- /other_cont/ l+ ## 0
