with Fail_But_Ok_Cond;

--  Test the coverage state when all conditions of a failing assertion are
--  executed.

procedure Test_Fail_But_Ok_Cond is
begin
   Fail_But_Ok_Cond;
end Test_Fail_But_Ok_Cond;

--# functions.ads
-- /foo_pre/         l- ## a-
-- /foo_post/        l- ## a-
-- /bar_expr/        l- ## s-
-- /bar_pre/         l- ## a-
-- /baz_expr/        l+ ## 0
--# functions.adb
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
-- # fail_but_ok_cond.adb
-- /assertion/       l! ## aT-
-- /catch/           l+ ## 0
