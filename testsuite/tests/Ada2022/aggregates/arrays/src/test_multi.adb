pragma Ada_2022;

with Pkg; use Pkg;

with Support; use Support;

procedure Test_Multi is
begin
   Assert (Absolute ([9, 4]) = [9, 4]);
end Test_Multi;

--# pkg.adb
--
-- /empty_aggr_guard/ l! ## dT-
-- /empty_aggr_st/    l- ## s-
-- /single_elt_guard/ l! ## dT-
-- /single_elt_st/    l- ## s-
-- /single_elt_dc/    l- ## 0
-- /multi_elt_st/     l+ ## 0
-- /multi_elt_dc/     l! ## dF-
