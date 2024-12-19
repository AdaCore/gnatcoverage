with Ada.Assertions;
with Expr_Func;

--  Test the correct assertion coverage reporting for an expression function
--  with a precondition and a postcondition expressed as aspects. This is
--  tested with and without a prior declaration.

procedure Test_Expr_Func is
begin
   begin
      Expr_Func;
   exception
      when Ada.Assertions.Assertion_Error => null;
   end;
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
