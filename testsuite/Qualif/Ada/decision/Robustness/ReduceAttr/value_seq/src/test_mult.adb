with Support; use Support;

with Pkg; use Pkg;

procedure Test_Mult is
   Inp : Arr_T := (1, 2, 3);
begin
   Assert (Compute (Inp, Abs_Mult) = 6);
end Test_Mult;

--# pkg.adb
--
-- /reducer_main/ l! ## dT-
-- /reducer_cont/ l+ ## 0
-- /reduce_stmt/  l+ ## 0
-- /filter/       l! ## dF-
-- /map/          l! ## dT-
-- /reduce_dc/    l! ## dT-
