pragma Ada_2022;

with Support; use Support;

with Pkg; use Pkg;

procedure Test_Full is
   Inp : Arr_T := [for I in 1 .. 3 => I];
begin
   Assert (Compute (Inp, Sum) / 2 = Compute (Inp, Max));
end Test_Full;

--# pkg.adb
--
-- /reducer_main/ l+ ## 0
-- /reducer_cont/ l+ ## 0
-- /reduce_st/    l+ ## 0
-- /reduce_dc/    l+ ## 0
