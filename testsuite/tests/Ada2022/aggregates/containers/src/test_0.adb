with Pkg;     use Pkg;
with Check;

procedure Test_0 is
begin
   null;
end Test_0;

--# pkg.adb
--
-- /empty_aggr_guard/ l- ## s-
-- /empty_aggr_st/    l- ## s-
-- /single_elt_guard/ l- ## s-
-- /single_elt_st/    l- ## s-
-- /single_elt_dc1/   l- ## s-
-- /single_elt_dc2/   l- ## 0
-- /multi_elt_st/     l- ## s-
-- /multi_elt_dc1/    l- ## 0
-- /multi_elt_dc2/    l- ## 0
