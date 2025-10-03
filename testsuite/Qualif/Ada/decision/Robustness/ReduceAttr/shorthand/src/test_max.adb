with Support; use Support;

with Pkg; use Pkg;

procedure Test_Max is
   Inp : Arr_T := (1, 3, 2, 5, 4);
begin
   Assert (Compute (Inp, Max) = 5);
end Test_Max;

--# pkg.adb
--
-- /reducer_main/ l! ## dT-
-- /reducer_cont/ l+ ## 0
-- /reduce_st/    l+ ## 0
-- /reduce_dc/    l! ## dT-
