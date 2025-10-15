with Make_Call;

--  Test function and call coverage on functions taking and returning types
--  with dotted names.

procedure Test_Visible is
begin
   Make_Call;
end Test_Visible;

--# make_call.adb
-- /fun/      l+ ## 0
-- /decl/     l+ ## 0
-- /call/     l+ ## 0
-- /if/       l+ ## 0
-- /d_f2/     l+ ## 0
-- /d_f1/     l! ## eT-
-- /d_call/   l! ## c-
-- /d_f3/     l! ## 0
-- /d_nf1/    l! ## 0
-- /deci/     l! ## 0
-- /v_dec/    l- ## 0
-- /false/    l+ ## 0
-- /null/     l- ## s-
