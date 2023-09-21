with Assertions_0;

procedure Test_Assertions_0 is
begin
   Assertions_0;
end Test_Assertions_0;

--# assertions.ads
-- /foo_pre/         l+ ## 0
-- /foo_post/        a=>l+, c=>l! ## a=>0, c=>ac!
-- /bar_expr/        l+ ## 0
-- /bar_pre/         a=>l+, c=>l! ## a=>0, c=>ac!
-- /baz_expr/        l- ## s-
--# assertions.adb
-- /foo_hi_decl/     l+ ## 0
-- /foo_loop_1/      l+ ## 0
-- /foo_inv_1/       l+ ## 0
-- /foo_loop_2/      l+ ## 0
-- /foo_inv_2/       a=>l+, c=>l! ## a=>0, c=>ac!
-- /foo_return/      l+ ## 0
-- /id_pre/          a=>l+, c=>l! ## a=>0, c=> ac!
-- /id_post/         l+ ## 0
-- /id_ret/          l+ ## 0
-- /nested_1/        a=>l+, c=>l! ## a=>0, c=>ac!
-- /nested_2/        a=>l+, c=>l! ## a=>0, c=>ac!
-- /nested_3/        l+ ## 0
-- /same_ret/        l+ ## 0
-- # assertions_0.adb
-- /assert_1/        a=>l+, c=>l! ## a=>0, c=>ac!
-- /foo_0/           l+ ## 0
-- /if_false/        l! ## dT-
-- /assert_2/        l- ## s-
-- /null/            l- ## s-
-- /bar_0/           l+ ## 0
