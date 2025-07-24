pragma Ada_2022;

with Pkg;   use Pkg;
with Check;

procedure Test_Empty is
   use Int_Sets;
begin
   Check ([]);
end Test_Empty;

--# pkg.adb
--
-- /empty_aggr_guard/ l! ## dF-
-- /empty_aggr_st/    l+ ## 0
-- /single_elt_guard/ l- ## s-
-- /single_elt_st/    l- ## s-
-- /single_elt_dc/    l- ## 0
-- /multi_elt_st/     l- ## s-
-- /multi_elt_dc/     l- ## 0
