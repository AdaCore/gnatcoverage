with Make_Call;

--  Test function and call coverage on functions taking and returning types
--  with dotted names.

procedure Test_Dotted_Type is
begin
   Make_Call;
end Test_Dotted_Type;

--# make_call.adb
-- /fun/      l+ ## 0
-- /decl/     l+ ## 0
-- /call/     l+ ## 0
-- /dummy4/   l? ## c?
-- /if/       l+ ## 0
-- /d_f2/     l+ ## 0
-- /d_f1/     l! ## eT-
-- /d_call/   l! ## c-
-- /d_f3/     l! ## 0
-- /d_nf1/    l! ## 0
-- /deci/     l! ## 0
-- /v_dec/    l- ## 0
-- /p_pkg5/   l! ## c?
-- /false/    l+ ## 0
-- /null/     l- ## s-
--# pkg4.ads
-- /decl/     l+ ## 0
--# pkg5.ads
-- /fun/      l+ ## 0
