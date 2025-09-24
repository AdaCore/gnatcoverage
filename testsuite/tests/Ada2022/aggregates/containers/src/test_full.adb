pragma Ada_2022;

with Pkg;   use Pkg;
with Check;

procedure Test_Full is
   use Int_Sets;
begin
   Check ([]);
   Check ([9]);
   Check ([-8]);
   Check ([-3, 2]);
end Test_Full;

--# pkg.adb
--
-- /empty_aggr_guard/ l+ ## 0
-- /empty_aggr_st/    l+ ## 0
-- /single_elt_guard/ l+ ## 0
-- /single_elt_st/    l+ ## 0
-- /single_elt_dc1/   l+ ## 0
-- /single_elt_dc2/   l+ ## 0
-- /multi_elt_st/     l+ ## 0
-- /multi_elt_dc1/    l+ ## 0
-- /multi_elt_dc2/    l+ ## 0
