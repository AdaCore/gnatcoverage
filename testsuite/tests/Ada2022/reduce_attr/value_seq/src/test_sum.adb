with Support; use Support;

with Pkg; use Pkg;

procedure Test_Sum is
   Inp : Arr_T := (1, 2, 3);
begin
   Assert (Compute (Inp, Sum) = 6);
end Test_Sum;

--# pkg.adb
--
-- /reducer_main/ l! ## dF-
-- /reducer_cont/ l+ ## 0
-- /reduce_stmt/  l+ ## 0
-- /filter/       l! ## dF-
-- /map/          l! ## dF-
-- /reduce_dc/    l! ## dF-
