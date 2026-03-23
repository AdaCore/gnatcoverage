with Make_Call;

--  Check that a subprogram called indirectly via an access through a call to
--  another subprogram yields correct function and call coverage results.

procedure Test_Call_1 is
begin
   Make_Call (1);
end Test_Call_1;

--# make_call.adb
-- /decl/   l+ ## 0
-- /arr_d/  l+ ## 0
-- /arr_i/  l+ ## 0
-- /fun/    l+ ## 0
-- /call_0/ l+ ## 0
-- /case/   l+ ## 0
-- /call_1/ l+ ## 0
-- /call_2/ l- ## s-,c-
-- /call_3/ l- ## s-,c-
-- /n_case/ l- ## s-

--# pkg.ads
-- /decl/   l+ ## 0
-- /set/    l+ ## 0
-- /reset/  l- ## s-,f-
