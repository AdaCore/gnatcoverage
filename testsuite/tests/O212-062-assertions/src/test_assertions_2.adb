with Catch_Assertions_2;

procedure Test_Assertions_2 is
begin
   Catch_Assertions_2;
end Test_Assertions_2;

--# assertions.ads
-- /foo_pre/         l- ## a-
-- /foo_post/        l- ## a-
-- /bar_expr/        l- ## s-
-- /bar_pre/         l- ## aT-
-- /baz_expr/        l- ## s-
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
--# assertions_2.adb
-- /foo_2/           l+ ## c!
