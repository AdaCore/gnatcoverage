with Support; use Support;

with Pkg; use Pkg;

procedure Test_Pkg_Z is
   Input_Arr : Arr_T := (1 => 0);
begin
   Assert (Non_Zero_Mult (Input_Arr) = 1);
end Test_Pkg_Z;

--# pkg.adb
--
-- /filter/     l! ## dT-
-- /loop_body/  l- ## s-
-- /other_stmt/ l+ ## 0
-- /other_cont/ l+ ## 0
