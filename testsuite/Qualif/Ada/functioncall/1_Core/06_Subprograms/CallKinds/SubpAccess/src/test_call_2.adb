with Make_Call;

--  Check that calling a subprogram by déreferencing an access value yields
--  correct function and call coverage results.

procedure Test_Call_2 is
begin
   Make_Call (2);
end Test_Call_2;

--# make_call.adb
-- /test/   l+ ## 0
-- /decl/   l+ ## 0
-- /arr_d/  l+ ## 0
-- /arr_i/  l+ ## 0
-- /fun/    l- ## f-
-- /call_0/ l- ## s-,c-
-- /case/   l+ ## 0
-- /call_1/ l- ## s-,c-
-- /call_2/ l+ ## 0
-- /call_3/ l- ## s-,c-
-- /n_case/ l- ## s-

--# pkg.ads
-- /decl/   l+ ## 0
-- /set/    l+ ## 0
-- /reset/  l- ## s-,f-
