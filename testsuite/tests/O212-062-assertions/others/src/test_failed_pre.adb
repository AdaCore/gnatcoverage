with Ada.Assertions;
with Silent_Last_Chance;

With Failed_Pre;

--  Check the coverage states when calling Foo with a parameter not satisfying
--  the its precondition.

procedure Test_Failed_Pre is
begin
   begin
      Failed_Pre;
   exception
      when Ada.Assertions.Assertion_Error => null;
   end;
end Test_Failed_Pre;

--# functions.ads
-- /foo_pre/         l- ## aT-
-- /foo_post/        l- ## a-
-- /bar_expr/        l- ## s-
-- /bar_pre/         l- ## a-
-- /baz_expr/        l- ## s-
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
--# failed_pre.adb
-- /foo_call/        l+ ## 0
