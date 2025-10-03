with Support; use Support;

with Pkg; use Pkg;

procedure Test_Sum is
   Inp : Arr_T := (1, 3, 2, 5, 4);
begin
   Assert (Compute (Inp, Sum) = 15);
end Test_Sum;

--# pkg.adb
--
-- /reducer_main/ l! ## dF-
-- /reducer_cont/ l+ ## 0
-- /reduce_st/    l+ ## 0
-- /reduce_dc/    l! ## dF-
