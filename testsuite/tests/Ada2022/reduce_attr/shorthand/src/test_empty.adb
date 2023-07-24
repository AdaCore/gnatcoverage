with Support; use Support;

with Pkg; use Pkg;

procedure Test_Empty is
   Inp : Arr_T := (1 .. 0 => 0);
begin
   Assert (Compute (Inp, Sum) = 0);
end Test_Empty;

--# pkg.adb
--
-- /reducer_main/ l- ## s-
-- /reducer_cont/ l- ## 0
-- /reduce_st/    l+ ## 0
-- /reduce_dc/    l! ## dF-
