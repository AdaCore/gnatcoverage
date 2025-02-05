with Support; use Support;

with Pkg; use Pkg;

procedure Test_Full is
   Inp : Arr_T := (1, 2, 0, 3);
begin
   Assert (Compute (Inp, Sum) = Compute (Inp, Abs_Mult));
end Test_Full;

--# pkg.adb
--
-- /reducer_main/ l+ ## 0
-- /reducer_cont/ l+ ## 0
-- /reduce_stmt/  l+ ## 0
-- /filter/       l+ ## 0
-- /map/          l+ ## 0
-- /reduce_dc/    l+ ## 0
