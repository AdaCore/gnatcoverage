with Assertions_1;

procedure Test_Assertions_1 is
begin
   Assertions_1;
end Test_Assertions_1;

--# assertions.ads
-- /foo_pre/         l- ## a-
-- /foo_post/        l- ## a-
-- /bar_expr/        l- ## s-
-- /bar_pre/         l- ## a-
-- /baz_expr/        l+ ## 0
--# assertions.adb
-- /foo_hi_decl/     l- ## s-
-- /foo_loop_1/      l- ## s-
-- /foo_inv_1/       l- ## s-
-- /foo_loop_2/      l- ## s-
-- /foo_inv_2/       l- ## s-
-- /foo_return/      l- ## s-
-- /id_pre/          l- ## a-
-- /id_post/         l- ## a-
-- /id_ret/          l- ## s-
-- /nested_1/        l- ## s-
-- /nested_2/        l- ## s-
-- /nested_3/        l- ## s-
-- /same_ret/        l- ## s-
-- # assertions_1.adb
-- /assert_3/        a=>l+, c=>l! ## a=>0, c=>aT-
-- /null/            l+ ## 0
-- /catch_1/         l- ## s-
