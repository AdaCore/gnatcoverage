with Catch_Expr_Func;

procedure Test_Expr_Func is
begin
    Catch_Expr_Func;
end Test_Expr_Func;

--# expr_func.adb
-- /foo_decl/   l+ ## 0
-- /foo_pre/    l+ ## 0
-- /foo_post/   l+ ## 0
-- /bar_pre/    l+ ## 0
-- /bar_post/   l- ## aT-
-- /bar_def/    l+ ## 0
-- /dummy_decl/ l+ ## 0
-- /foo_call/   l+ ## 0
-- /bar_call/   l+ ## 0
