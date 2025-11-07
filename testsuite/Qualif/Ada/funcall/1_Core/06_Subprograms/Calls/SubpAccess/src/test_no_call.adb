with Make_Call;

--  Check that different ways of calling a subprogram via an access yields
--  correct function and call coverage results.

procedure Test_No_Call is
begin
   null;
end Test_No_Call;

--# make_call.adb
-- /test/   l- ## f-
-- /decl/   l- ## s-
-- /arr_d/  l- ## s-
-- /arr_i/  l- ## 0
-- /fun/    l- ## f-
-- /call_0/ l- ## s-,c-
-- /case/   l- ## s-
-- /call_1/ l- ## s-,c-
-- /call_2/ l- ## s-,c-
-- /call_3/ l- ## s-,c-
-- /n_case/ l- ## s-

--# pkg.ads
-- /decl/   l+ ## 0
-- /set/    l- ## s-,f-
-- /reset/  l- ## s-,f-
