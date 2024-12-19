with Various_Tests;

--  Test various assertion scenarios. The details can be found directly in
--  various_tests.adb.

procedure Test_Various_Tests is
begin
   Various_Tests;
end Test_Various_Tests;

--# functions.ads
-- /foo_pre/         l+ ## 0
-- /foo_post/        a=>l+, c=>l! ## a=>0, c=>ac!
-- /bar_expr/        l+ ## 0
-- /bar_pre/         a=>l+, c=>l! ## a=>0, c=>ac!
-- /baz_expr/        l- ## s-
--# functions.adb
-- /foo_hi_decl/     l+ ## 0
-- /foo_loop_1/      l+ ## 0
-- /foo_inv_1/       l+ ## 0
-- /foo_loop_2/      l+ ## 0
-- /foo_inv_2/       a=>l+, c=>l! ## a=>0, c=>ac!
-- /foo_return/      l+ ## 0
-- /id_pre/          a=>l+, c=>l! ## a=>0, c=>ac!,ac!
-- /id_post/         l+ ## 0
-- /id_ret/          l+ ## 0
-- /nested_1/        a=>l+, c=>l! ## a=>0, c=>ac!,ac!
-- /nested_2/        a=>l+, c=>l! ## a=>0, c=>ac!
-- /nested_3/        l+ ## 0
-- /same_ret/        l+ ## 0
-- # various_tests.adb
-- /assert_1/        a=>l+, c=>l! ## a=>0, c=>ac!,ac!
-- /foo/             l+ ## 0
-- /if_false/        l! ## dT-
-- /assert_2/        l- ## s-
-- /bar/             l+ ## 0
