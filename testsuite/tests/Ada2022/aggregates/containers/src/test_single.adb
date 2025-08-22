pragma Ada_2022;

with Pkg;   use Pkg;
with Check;

procedure Test_Single is
   use Int_Sets;
begin
   Check ([9]);
end Test_Single;

--# pkg.adb
--
-- /empty_aggr_guard/ l! ## dT-
-- /empty_aggr_st/    l- ## s-
-- /single_elt_guard/ l! ## dF-
-- /single_elt_st/    l+ ## 0
-- /single_elt_dc1/   l! ## dF-
-- /single_elt_dc2/   l! ## dF-
-- /multi_elt_st/     l- ## s-
-- /multi_elt_dc1/    l- ## 0
-- /multi_elt_dc2/    l- ## 0
