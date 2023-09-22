with Ada.Assertions;
with Expr_Func;

procedure Catch_Expr_Func is
begin
   begin
      Expr_Func;
   exception
      when Ada.Assertions.Assertion_Error => null;
   end;
end Catch_Expr_Func;
