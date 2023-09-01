pragma Ada_2022;

with Pkg; use Pkg;

with Support; use Support;

procedure Test_Full is
begin
   Assert (Absolute ([]) = []);
   Assert (Absolute ([9]) = [9]);
   Assert (Absolute ([-8]) = [8]);
   Assert (Absolute ([-3, 2]) = [3, 2]);
end Test_Full;

--# pkg.adb
--
-- /empty_aggr_guard/ l+ ## 0
-- /empty_aggr_st/    l+ ## 0
-- /single_elt_guard/ l+ ## 0
-- /single_elt_st/    l+ ## 0
-- /single_elt_dc/    l+ ## 0
-- /multi_elt_st/     l+ ## 0
-- /multi_elt_dc/     l+ ## 0
