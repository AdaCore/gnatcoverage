with Make_Call;

--  Test that a call to a subprogram made via an access value stored in an
--  array yields correct function and call coverage results.

procedure Test_Call_3 is
begin
   Make_Call (3);
end Test_Call_3;

--# make_call.adb
-- /test/   l+ ## 0
-- /decl/   l+ ## 0
-- /arr_d/  l+ ## 0
-- /arr_i/  l+ ## 0
-- /fun/    l- ## f-
-- /call_0/ l- ## s-,c-
-- /case/   l+ ## 0
-- /call_1/ l- ## s-,c-
-- /call_2/ l- ## s-,c-
-- /call_3/ l+ ## 0
-- /n_case/ l- ## s-

--# pkg.ads
-- /decl/   l+ ## 0
-- /set/    l- ## s-,f-
-- /reset/  l+ ## 0
